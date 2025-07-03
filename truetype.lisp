;;; truetype.lisp
(defpackage #:clx/truetype
  (:nicknames #:xft)
  (:use #:cl :std :dat/ttf)
  (:export
   :drawable-screen
   :font-ascent
   :font-descent
   :text-bounding-box
   :screen-default-dpi
   :screen-dpi
   :draw-text
   :draw-text-line
   :text-height
   :text-width
   :text-line-bounding-box
   :text-line-width
   :text-line-height
   :font-line-gap
   :baseline-to-baseline
   :font-lines-height
   :*allow-fixed-pitch-p*)
  (:documentation "Package contains API for TrueType text rendering using CLX, XRender.
Glyphs information is obtained by DAT/TTF. Font rasterization is made by CL-VECTORS."))

(in-package #:clx/truetype)

;;; Utils
(defun drawable-screen (drawable)
  (typecase drawable
    (xlib:drawable
     (dolist (screen (xlib:display-roots (xlib:drawable-display drawable)))
       (when (xlib:drawable-equal (xlib:screen-root screen) (xlib:drawable-root drawable))
         (return screen))))
    (xlib:screen drawable)
    (t nil)))

;;; Font Cache
(defun make-font-cache (font dpi-cache-size string-cache-size inner-provider)
  (flet ((outer-provider (dpi-cons)
           (values
            (cacle:make-cache
             string-cache-size
             (lambda (string)
               (values
                (funcall inner-provider (car dpi-cons) (cdr dpi-cons) font string)
                (length string)))
             :test #'equal
             :policy :lfu)
            1)))
    (cacle:make-cache
     dpi-cache-size
     #'outer-provider
     :test #'equal
     :policy :lfu)))

(defun font-cache-fetch (cache dpi string)
  (cacle:cache-fetch (cacle:cache-fetch cache dpi) string))

(defmethod initialize-instance :after
    ((font font) &key (dpi-cache-size 10) (string-cache-size 1000) &allow-other-keys)
  (setf
   (font-string-bboxes font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-bounding-box-provider)
   (font-string-line-bboxes font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-line-bounding-box-provider)
   (font-string-alpha-maps font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-pixarray-provider)
   (font-string-line-alpha-maps font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-line-pixarray-provider)))

;;; Screen DPI
(defun screen-default-dpi (screen)
  "Returns default dpi for @var{screen}. pixel width * 25.4/millimeters width"
  (values (floor (* (xlib:screen-width screen) 25.4)
                 (xlib:screen-width-in-millimeters screen))
          (floor (* (xlib:screen-height screen) 25.4)
                 (xlib:screen-height-in-millimeters screen))))

(defun screen-dpi (screen)
  "Returns current dpi for @var{screen}."
  (values (getf (xlib:screen-plist screen) :dpi-x
                (floor (* (xlib:screen-width screen) 25.4)
                       (xlib:screen-width-in-millimeters screen)))
          (getf (xlib:screen-plist screen) :dpi-y
                (floor (* (xlib:screen-height screen) 25.4)
                             (xlib:screen-height-in-millimeters screen)))))

(defun (setf screen-dpi) (value screen)
  "Sets current dpi for @var{screen}."
  (setf (getf (xlib:screen-plist screen) :dpi-x) value
        (getf (xlib:screen-plist screen) :dpi-y) value))

;;; Font metrics
(defun font-units->pixels-x (dpi-x font)
  "px = funits*coeff. Function returns coeff."
  (with-font-loader (loader font)
    (with-slots (size) font
      (let* ((units/em (ttf:units/em loader))
             (pixel-size-x (* size (/ dpi-x 72))))
        (* pixel-size-x (/ units/em))))))

(defun font-units->pixels-y (dpi-y font)
  "px = funits*coeff. Function returns coeff."
  (with-font-loader (loader font)
    (with-slots (size) font
      (let* ((units/em (ttf:units/em loader))
             (pixel-size-y (* size (/ dpi-y 72))))
        (* pixel-size-y (/ units/em))))))

(defun font-ascent-for-dpi (dpi-y font)
  (with-font-loader (loader font)
    (ceiling (* (font-units->pixels-y dpi-y font) (ttf:ascender loader)))))

(defun font-descent-for-dpi (dpi-y font)
  (with-font-loader (loader font)
    (floor (* (font-units->pixels-y dpi-y font) (ttf:descender loader)))))

(defun font-ascent (drawable font)
  "Returns ascent of @var{font}. @var{drawable} must be window, pixmap or screen."
  (font-ascent-for-dpi (nth-value 1 (screen-dpi (drawable-screen drawable))) font))

(defun font-descent (drawable font)
  "Returns descent of @var{font}. @var{drawable} must be window, pixmap or screen."
  (font-descent-for-dpi (nth-value 1 (screen-dpi (drawable-screen drawable))) font))

(defun font-line-gap (drawable font)
  "Returns line gap of @var{font}. @var{drawable} must be window, pixmap or screen."
  (with-font-loader (loader font)
    (ceiling (* (font-units->pixels-y drawable font) (ttf:line-gap loader)))))

;;; baseline-to-baseline = ascent - descent + line gap
(defun baseline-to-baseline (drawable font)
  "Returns distance between baselines of @var{font}. @var{drawable} must be window, pixmap or screen. ascent - descent + line gap"
  (+ (font-ascent drawable font) (- (font-descent drawable font))
     (font-line-gap drawable font)))

(defun text-bounding-box-provider (dpi-x dpi-y font string)
  (with-font-loader (loader font)
    (let* ((bbox (ttf:string-bounding-box string loader))
           (units->pixels-x (font-units->pixels-x dpi-x font))
           (units->pixels-y (font-units->pixels-y dpi-y font))
           (xmin (ttf:bbox-xmin bbox))
           (ymin (ttf:bbox-ymin bbox))
           (xmax (ttf:bbox-xmax bbox))
           (ymax (ttf:bbox-ymax bbox)))
      (when (font-underline font)
        (setf ymin (min ymin (- (ttf:underline-position loader)
                                (ttf:underline-thickness loader)))))
      (when (font-overline font)
        (setf ymax (max ymax (+ (ttf:ascender loader)
                                (ttf:underline-position loader)
                                (+ (ttf:underline-thickness loader))))))
      (vector (floor (* xmin
                        units->pixels-x))
              (floor (* ymin
                        units->pixels-y))
              (ceiling (* xmax
                          units->pixels-x))
              (ceiling (* ymax
                          units->pixels-y))))))

(defun text-bounding-box (drawable font string &key start end)
  "Returns text bounding box. @var{drawable} must be window, pixmap or screen. Text bounding box is only for contours. Bounding box for space (#x20) is zero."
  (when (and start end)
    (setf string (subseq string start end)))
  (font-cache-fetch (font-string-bboxes font)
                    (multiple-value-call #'cons (screen-dpi (drawable-screen drawable)))
                    string))

(defun text-width (drawable font string &key start end)
  "Returns width of text bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (bbox-xmax bbox) (xmin bbox))))

(defun text-height (drawable font string &key start end)
  "Returns height of text bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (ymax bbox) (ymin bbox))))

(defvar *allow-fixed-pitch-p* t
  "Allow shortcutting fixed-pitch fonts. Somewhat faster, but breaks CJK
in fixed-pitch fonts.")

(defun text-line-bounding-box-provider (dpi-x dpi-y font string)
  (with-font-loader (loader font)
    (let* ((units->pixels-x (font-units->pixels-x dpi-x font))
           (xmin 0)
           (ymin (font-descent-for-dpi dpi-y font))
           (ymax (font-ascent-for-dpi dpi-y font))
           (string-length (length string))
           (xmax (if (> string-length 0)
                     (ttf:advance-width (ttf:find-glyph (elt string 0) loader))
                     0)))
      (if (and *allow-fixed-pitch-p* (ttf:fixed-pitch-p loader))
          (setf xmax (* xmax string-length))
          (do ((i 1 (1+ i)))
              ((>= i string-length))
            (incf xmax
                  (+ (ttf:advance-width (ttf:find-glyph (elt string i) loader))
                     (ttf:kerning-offset (elt string (1- i)) (elt string i) loader)))))
      (vector (floor (* xmin units->pixels-x))
              ymin
              (ceiling (* xmax
                          units->pixels-x))
              ymax))))

(defun text-line-bounding-box (drawable font string &key start end)
  "Returns text line bounding box. @var{drawable} must be window, pixmap or screen. Text line bounding box is bigger than text bounding box. It's height is ascent + descent, width is sum of advance widths minus sum of kernings."
  (when (and start end)
    (setf string (subseq string start end)))
  (font-cache-fetch (font-string-line-bboxes font)
                    (multiple-value-call #'cons (screen-dpi (drawable-screen drawable)))
                    string))

(defun text-line-width (drawable font string &key start end)
  "Returns width of text line bounding box. @var{drawable} must be window, pixmap or screen. It is sum of advance widths minus sum of kernings."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (bbox-xmax bbox) (xmin bbox))))

(defun text-line-height (drawable font string &key start end)
  "Returns height of text line bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (bbox-ymax bbox) (bbox-ymin bbox))))

(defun xmin (bounding-box)
  "Returns left side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 0))))

(defun ymin (bounding-box)
  "Returns bottom side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 1))))

(defun xmax (bounding-box)
  "Returns right side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 2))))

(defun ymax (bounding-box)
  "Returns top side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 3))))

;;; Font rendering 
(defun clamp (value min max)
  "Clamps the value 'value' into the range [min,max]."
  (max min (min max value)))

(defun make-state (font)
  "Wrapper around antialising and not antialiasing renderers."
  (if (font-antialias font)
      (aa:make-state)
      (aa-bin:make-state)))

(defun aa-bin/update-state (state paths)
  "Update state for not antialiasing renderer."
  (if (listp paths)
      (dolist (path paths)
        (aa-bin/update-state state path))
      (let ((iterator (paths:path-iterator-segmented paths)))
        (multiple-value-bind (i1 k1 e1) (paths:path-iterator-next iterator)
          (declare (ignore i1))
          (when (and k1 (not e1))
            ;; at least 2 knots
            (let ((first-knot k1))
              (loop
                 (multiple-value-bind (i2 k2 e2) (paths:path-iterator-next iterator)
                   (declare (ignore i2))
                   (aa-bin:line-f state
                           (paths:point-x k1) (paths:point-y k1)
                           (paths:point-x k2) (paths:point-y k2))
                   (setf k1 k2)
                   (when e2
                     (return))))
              (aa-bin:line-f state
                      (paths:point-x k1) (paths:point-y k1)
                      (paths:point-x first-knot) (paths:point-y first-knot)))))))
  state)

(defun update-state (font state paths)
  "Wrapper around antialising and not antialiasing renderers."
  (if (font-antialias font)
      (vectors:update-state state paths)
      (aa-bin/update-state state paths)))

(defun cells-sweep (font state function &optional function-span)
  "Wrapper around antialising and not antialiasig renderers."
  (if (font-antialias font)
      (aa:cells-sweep state function function-span)
      (aa-bin:cells-sweep state function function-span)))

(defun text-pixarray-provider (dpi-x dpi-y font string)
  (with-font-loader (font-loader font)
    (let* ((bbox (font-cache-fetch (font-string-bboxes font) (cons dpi-x dpi-y) string))
           (min-x (xmin bbox))
           (min-y (ymin bbox))
           (max-x (xmax bbox))
           (max-y (ymax bbox))
           (width  (- max-x min-x))
           (height (- max-y min-y)))
      (if (or (= 0 width) (= 0 height))
          (list nil 0 0 0 0)
          (let* ((units->pixels-x (font-units->pixels-x dpi-x font))
                 (units->pixels-y (font-units->pixels-y dpi-y font))
                 (array (make-array (list height width)
                                    :initial-element 0
                                    :element-type '(unsigned-byte 8)))
                 (state (make-state font))
                 (paths (paths-ttf:paths-from-string font-loader string
                                                     :offset (paths:make-point (- min-x)
                                                                               max-y)
                                                     :scale-x units->pixels-x
                                                     :scale-y (- units->pixels-y))))

            (when (font-underline font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* units->pixels-y (ttf:underline-position font-loader)))
                     (underline-path (paths:make-rectangle-path 0 (+ max-y (- underline-offset))
                                                                max-x (+ max-y (- underline-offset) thickness))))
                (push underline-path paths)))
            (when (font-strikethrough font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* 2 units->pixels-y (ttf:underline-position font-loader)))
                     (line-path (paths:make-rectangle-path 0 (+ max-y underline-offset) max-x (+ max-y underline-offset thickness))))
                (push line-path paths)))
            (when (font-overline font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* units->pixels-y (ttf:underline-position font-loader)))
                     (ascend (* units->pixels-y (ttf:ascender font-loader)))
                     (overline-path (paths:make-rectangle-path 0 (- max-y ascend underline-offset)
                                                               max-x
                                                               (- max-y ascend underline-offset thickness))))
                (push overline-path paths)))
            (update-state font state paths)
            (cells-sweep font state
                         (lambda (x y alpha)
                           (when (and (<= 0 x (1- width))
                                      (<= 0 y (1- height)))
                             (setf alpha (min 255 (abs alpha))
                                   (aref array y x) (clamp
                                                     (floor (+ (* (- 256 alpha) (aref array y x))
                                                               (* alpha 255))
                                                            256)
                                                     0 255)))))
            (list array
                  min-x
                  max-y
                  width
                  height))))))

(defun text-pixarray (drawable font string)
  "Render a text string of 'face', returning a 2D (unsigned-byte 8) array 
suitable as an alpha mask, and dimensions. This function returns five
values: alpha mask byte array, x-origin, y-origin (subtracted from
position before rendering), horizontal and vertical advances.
@var{drawable} must be window or pixmap."
  (values-list
   (font-cache-fetch
    (font-string-alpha-maps font)
    (multiple-value-call #'cons (screen-dpi (drawable-screen drawable)))
    string)))

(defun text-line-pixarray-provider (dpi-x dpi-y font string)
  (with-font-loader (font-loader font)
    (let* ((bbox (font-cache-fetch (font-string-line-bboxes font) (cons dpi-x dpi-y) string))
           (min-x (xmin bbox))
           (min-y (ymin bbox))
           (max-x (xmax bbox))
           (max-y (ymax bbox))
           (width  (- max-x min-x))
           (height (- max-y min-y)))
      (if (or (= 0 width) (= 0 height))
          (list nil 0 0 0 0)
          (let* ((units->pixels-x (font-units->pixels-x dpi-x font))
                 (units->pixels-y (font-units->pixels-y dpi-y font))
                 (array (make-array (list height width)
                                    :initial-element 0
                                    :element-type '(unsigned-byte 8)))
                 (state (make-state font))
                 (paths (paths-ttf:paths-from-string font-loader string
                                                     :offset (paths:make-point (- min-x)
                                                                               max-y)
                                                     :scale-x units->pixels-x
                                                     :scale-y (- units->pixels-y))))
            (when (font-underline font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* units->pixels-y (ttf:underline-position font-loader)))
                     (underline-path (paths:make-rectangle-path 0 (+ max-y (- underline-offset))
                                                                max-x (+ max-y (- underline-offset) thickness))))
                (push underline-path paths)))
            (when (font-strikethrough font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* 2 units->pixels-y (ttf:underline-position font-loader)))
                     (line-path (paths:make-rectangle-path 0 (+ max-y underline-offset) max-x (+ max-y underline-offset thickness))))
                (push line-path paths)))
            (when (font-overline font)
              (let* ((thickness (* units->pixels-y (ttf:underline-thickness font-loader)))
                     (underline-offset (* units->pixels-y (ttf:underline-position font-loader)))
                     (ascend (* units->pixels-y (ttf:ascender font-loader)))
                     (overline-path (paths:make-rectangle-path 0 (- max-y ascend underline-offset)
                                                               max-x
                                                               (- max-y ascend underline-offset thickness))))
                (push overline-path paths)))
            (update-state font state paths)
            (cells-sweep font state
                         (lambda (x y alpha)
                           (when (and (<= 0 x (1- width))
                                      (<= 0 y (1- height)))
                             (setf alpha (min 255 (abs alpha))
                                   (aref array y x) (clamp
                                                     (floor (+ (* (- 256 alpha) (aref array y x))
                                                               (* alpha 255))
                                                            256)
                                                     0 255)))))
            (list array 
                  min-x
                  max-y
                  width
                  height))))))

(defun text-line-pixarray (drawable font string)
  "Render a text line of 'face', returning a 2D (unsigned-byte 8) array
suitable as an alpha mask, and dimensions. This function returns five
values: alpha mask byte array, x-origin, y-origin (subtracted from
position before rendering), horizontal and vertical advances.
@var{drawable} must be window or pixmap."
  (values-list
   (font-cache-fetch
    (font-string-line-alpha-maps font)
    (multiple-value-call #'cons (screen-dpi (drawable-screen drawable)))
    string)))

(defun update-foreground (drawable gcontext font)
  "Lazy updates foreground for drawable. @var{drawable} must be window or pixmap."
  (let ((pixmap (or (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                    (setf (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                          (xlib:create-pixmap 
                           :drawable drawable
                           :depth (xlib:drawable-depth drawable)
                           :width 1 :height 1)))))
    (let ((color (the xlib:card32 
                      (if (font-overwrite-gcontext font)
                          (font-foreground font)
                          (xlib:gcontext-foreground gcontext)))))
      (when (or (null (getf (xlib:drawable-plist drawable) :ttf-foreground))
                (/= (getf (xlib:drawable-plist drawable) :ttf-foreground)
                    color))
        (let ((previous-color (xlib:gcontext-foreground gcontext)))
          (setf (xlib:gcontext-foreground gcontext) color)
          (xlib:draw-point pixmap gcontext 0 0)
          (setf (xlib:gcontext-foreground gcontext) previous-color)
          (setf (getf (xlib:drawable-plist drawable) :ttf-foreground) color))))))

(defun update-background (drawable gcontext font x y width height)
  "Lazy updates background for drawable. @var{drawable} must be window or pixmap."
  (let ((previous-color (xlib:gcontext-foreground gcontext))
        (color (the xlib:card32 
                    (if (font-overwrite-gcontext font)
                        (font-background font)
                        (xlib:gcontext-background gcontext)))))
    (setf (xlib:gcontext-foreground gcontext) color)
    (xlib:draw-rectangle drawable gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) previous-color)))

;;; Caching X11 objects
(defun get-drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-surface)
      (setf (getf (xlib:drawable-plist drawable) :ttf-surface)
            (xlib:render-create-picture 
             drawable 
             :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
                                                                 :depth (xlib:drawable-depth drawable)))))))

(defun get-drawable-pen-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-pen)
      (setf (getf (xlib:drawable-plist drawable) :ttf-pen)
            (xlib:render-create-picture
             (or (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                 (setf (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                       (xlib:create-pixmap 
                        :drawable drawable
                        :depth (xlib:drawable-depth drawable)
                        :width 1 :height 1)))
             :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
                                                                 :depth (xlib:drawable-depth drawable)))
             :repeat :on))))

(defun display-alpha-picture-format (display)
  (or (getf (xlib:display-plist display) :ttf-alpha-format)
      (setf (getf (xlib:display-plist display) :ttf-alpha-format)
            (first
             (xlib:find-matching-picture-formats
              display
              :depth 8 :alpha 8 :red 0 :blue 0 :green 0)))))

;;; Drawing text

(defun draw-text (drawable gcontext font string x y &key start end 
                                                      draw-background-p)
  "Draws text string using @var{font} on @var{drawable} with graphic context @var{gcontext}. @var{x}, @var{y} are the left point of base line. @var{start} and @var{end} are used for substring rendering.
If @var{gcontext} has background color, text bounding box will be filled with it. Text line bounding box is bigger than text bounding box. @var{drawable} must be window or pixmap."
  (when (and start end)
    (when (>= start end)
      (return-from draw-text))
    (setf string (subseq string start end)))
  (multiple-value-bind (alpha-data min-x max-y width height)
      (text-pixarray drawable font string)
    (when (or (= 0 width) (= 0 height))
      (return-from draw-text))
    (let* ((display (xlib:drawable-display drawable))
           (image (xlib:create-image :width width :height height :depth 8 :data alpha-data))
           (alpha-pixmap (xlib:create-pixmap :width width :height height :depth 8 :drawable drawable))
           (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
           (alpha-picture
             (progn
               (xlib:put-image alpha-pixmap alpha-gc image :x 0 :y 0)
               (xlib:render-create-picture alpha-pixmap
                                           :format (display-alpha-picture-format display))))
           (source-picture (get-drawable-pen-picture drawable))
           (destination-picture (get-drawable-picture drawable)))

      (xlib:free-gcontext alpha-gc)

      (update-foreground drawable gcontext font)
      (when draw-background-p
        (update-background drawable gcontext font (+ x min-x) (- y max-y) width height))
      ;; Sync the destination picture with the gcontext
      (setf (xlib:picture-clip-x-origin destination-picture) (xlib:gcontext-clip-x gcontext))
      (setf (xlib:picture-clip-y-origin destination-picture) (xlib:gcontext-clip-y gcontext))
      (setf (xlib:picture-subwindow-mode destination-picture) (xlib:gcontext-subwindow-mode gcontext))
      (setf (xlib::picture-clip-mask destination-picture)
            (xlib::gcontext-clip-mask gcontext))
      (xlib:render-composite :over source-picture alpha-picture destination-picture 0 0 0 0 (+ x min-x) (- y max-y) width height)

      (xlib:render-free-picture alpha-picture)
      (xlib:free-pixmap alpha-pixmap)
      nil)))

(defun draw-text-line (drawable gcontext font string x y &key start end draw-background-p)
  "Draws text string using @var{font} on @var{drawable} with graphic context @var{gcontext}. @var{x}, @var{y} are the left point of base line. @var{start} and @var{end} are used for substring rendering.
If @var{gcontext} has background color, text line bounding box will be filled with it. Text line bounding box is bigger than text bounding box. @var{drawable} must be window or pixmap."
  (when (and start end)
    (when (>= start end)
      (return-from draw-text-line))
    (setf string (subseq string start end)))
  (multiple-value-bind (alpha-data min-x max-y width height)
      (text-line-pixarray drawable font string)
    (when (or (= 0 width) (= 0 height))
      (return-from draw-text-line))
    (let* ((display (xlib:drawable-display drawable))
           (image (xlib:create-image :width width :height height :depth 8 :data alpha-data))

           (alpha-pixmap (xlib:create-pixmap :width width :height height :depth 8 :drawable drawable))
           (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
           (alpha-picture
             (progn
               (xlib:put-image alpha-pixmap alpha-gc image :x 0 :y 0)
               (xlib:render-create-picture alpha-pixmap 
                                           :format (display-alpha-picture-format display))))
           (source-picture (get-drawable-pen-picture drawable))
           (destination-picture (get-drawable-picture drawable)))

      (xlib:free-gcontext alpha-gc)

      (update-foreground drawable gcontext font)
      (when draw-background-p
        (update-background drawable gcontext font (+ x min-x) (- y max-y) width height))
      ;; Sync the destination picture with the gcontext
      (setf (xlib:picture-clip-x-origin destination-picture) (xlib:gcontext-clip-x gcontext))
      (setf (xlib:picture-clip-y-origin destination-picture) (xlib:gcontext-clip-y gcontext))
      (setf (xlib:picture-subwindow-mode destination-picture) (xlib:gcontext-subwindow-mode gcontext))
      (setf (xlib::picture-clip-mask destination-picture)
            (xlib::gcontext-clip-mask gcontext))
      (xlib:render-composite :over source-picture alpha-picture destination-picture 0 0 0 0 (+ x min-x) (- y max-y) width height)

      (xlib:render-free-picture alpha-picture)
      (xlib:free-pixmap alpha-pixmap)
      nil)))

;;; Test utils

(defun trgrey (i)
  "Visualize alpha mask using graphic characters"
  (cond
    ((> i 200) "██")
    ((> i 150) "▓▓")
    ((> i 100) "▒▒")
    ((> i 50)  "░░")
    (t "  ")))

(defun print-pixarray (array)
  "Print 2d array of alpha mask using graphic characters."
  (do ((i 0 (1+ i)))
      ((>= i (array-dimension array 0)) nil)
    (do ((j 0 (1+ j)))
        ((>= j (array-dimension array 1)) nil)
      (format t "~A" (trgrey (aref array i j))))
    (format t "~%")))

(defun font-lines-height (drawable font lines-count)
  "Returns text lines height in pixels. For one line height is ascender+descender. For more than one line height is ascender+descender+linegap."
  (if (> lines-count 0)
      (+ (+ (xft:font-ascent drawable font)
            (- (xft:font-descent drawable font)))
         (* (1- lines-count) (+ (xft:font-ascent drawable font)
                                (- (xft:font-descent drawable font))
                                (xft:font-line-gap drawable font))))
      0))
