;;;; Created on 2008-04-15 13:01:59

(in-package :clone-ml)

(defstruct onetype 
  (name nil)
  (id nil)
  (vals nil)
  (kind))


(defstruct item-attribute
  (name)
  (onetype))

(defstruct item-issue
  (name)
  (attributes))


(defstruct item
  (name nil)
  (attributes nil)
  (issues nil))

(defstruct im
  (name nil)
  (item)
  (types))

(defstruct model
  (name)
  (version)
  (im)
  (types))


(defun model-get-type (model typeid)
  (let ((el (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (im-types (model-im model)))))
    (if el
        (values el t)
        (values (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (model-types model)) nil))))

(defun load-model (filename)
  (let* ((model (make-model))
         (infomodel (setf (model-im model) (make-im)))
         (item (setf (im-item infomodel) (make-item))))
  ;(cxml:with-namespace ("xmi" "http://www.omg.org/XMI")
  ;                     (cxml:with-namespace ("negmod" "http://NegotiationMetaModel_v1.3.1.ecore")
    (xspam:with-xspam-source filename
      (xspam:element |NegotiationModel|
        (xspam:attribute |name|
            (setf (model-name model) xspam:_))
        (xspam:attribute |version|
            (setf (model-version model) xspam:_))
        (xspam:element |informationModel|
          (xspam:attribute |name|
            (setf (im-name infomodel) xspam:_))
          (xspam:element |item|
            (xspam:attribute |name|
              (setf (item-name item) xspam:_))
            (xspam:zero-or-more
             (xspam:one-of
              (xspam:element |attribute|
                (let ((attr (make-item-attribute)))
                  (xspam:attribute |name|
                    (setf (item-attribute-name attr) xspam:_)
                    (xspam:attribute |type|
                      (setf (item-attribute-onetype attr) xspam:_)))
                  (push attr (item-attributes item))))
              (xspam:element |issue|
                (let ((issue (make-item-issue)))
                  (xspam:attribute |name|
                    (setf (item-issue-name issue) xspam:_))
                  (xspam:zero-or-more
                   (xspam:element |attribute|
                     (let ((attr (make-item-attribute)))
                       (xspam:attribute |name|
                         (setf (item-attribute-name attr) xspam:_)
                         (xspam:attribute |type|
                           (setf (item-attribute-onetype attr) xspam:_)))
                       (push attr (item-issue-attributes issue)))))
                  (push issue (item-issues item)))))))
          (xspam:zero-or-more
           (xspam:element |enumeration|
             (let ((type (make-onetype :kind 'enum)))
               (xspam:attribute |type|
                 (setf (onetype-name type) xspam:_))
               (xspam:attribute |id|
                 (setf (onetype-id type) xspam:_))
               (xspam:zero-or-more
                (xspam:element |literal|
                  (xspam:attribute |name|
                    (push xspam:_ (onetype-vals type)))))
               (push type (im-types infomodel))))))
        (xspam:zero-or-more
         (xspam:one-of
          (xspam:element |primitiveType|
            (let ((type (make-onetype :kind 'primitive)))
              (xspam:attribute |type|
                (setf (onetype-name type) xspam:_))
              (xspam:attribute |id|
                (setf (onetype-id type) xspam:_))
              (push type (model-types model))))
          (xspam:element |complexType|
            (let ((type (make-onetype :kind 'complex)))
              (xspam:attribute |type|
                (setf (onetype-name type) xspam:_))
              (xspam:attribute |id|
                (setf (onetype-id type) xspam:_))
              (push type (model-types model))))
          (xspam:element |enumeration|
            (let ((type (make-onetype :kind 'enum)))
              (xspam:attribute |type|
                (setf (onetype-name type) xspam:_))
              (xspam:attribute |id|
                (setf (onetype-id type) xspam:_))
              (xspam:zero-or-more
               (xspam:element |literal|
                 (xspam:attribute |name|
                   (push xspam:_ (onetype-vals type)))))
              (push type (model-types model))))))))
    model))
