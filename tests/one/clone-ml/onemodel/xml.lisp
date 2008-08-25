;;;; Created on 2008-08-25 11:03:10

;;;; Created on 2008-08-18 11:41:45

(in-package :clone-ml)

;
; protege model import 
;

(defun onemodel-import (pathname)
  (let* ((model (make-model))
         (infomodel (make-im))
         (item (make-item)) 
         (seed (make-seed :model model :infomodel infomodel :item item)))
    (setf (model-im model) infomodel)
    (setf (im-item infomodel) item)
    (with-open-file (strm pathname :direction :input)
                    (s-xml:start-parse-xml strm
                                           (make-instance 's-xml:xml-parser-state
                                                          :seed seed
                                                          :new-element-hook #'model-import-new-element-hook
                                                          :finish-element-hook #'model-import-finish-element-hook
                                                          :text-hook #'model-import-text-hook)))
    (let* ((model (seed-model seed))
           (case-prefix (format nil "~A~A" (model-name model) (model-version model)))
           (kb (mlcl-kb:find-kb case-prefix)))
      (if (null kb)
          (setf kb (make-kb case-prefix 
                            :use (list 
                                  mlcl-kbs::PROTEGE-KB 
                                  mlcl-kbs::negotiation-kb
                                  mlcl-kbs::onenegotiation-kb)
                            :protege-file (merge-pathnames
                                           (make-pathname
                                            :directory '(:relative ".")
                                            :name case-prefix :type "xml" :case :local)
                                           *default-one-model-kb-pathname*)))
          (mlcl-kb:kb-clear kb))
      (convert-one-model model kb)
      (kb-save kb)
      kb)))

;
; namespaces
;

(in-package :mlcl-kb)

(defpackage :onemodel-ns
  (:export
    |primitiveType|
    |NegotiationModel|
   ))

(defpackage :xmi-ns
  (:export
    |id|
    |type|
    |name|
   ))

(s-xml:register-namespace "http://www.omg.org/XMI" "xmi" :xmi-ns)
(s-xml:register-namespace "http://NegotiationMetaModel_v1.3.1.ecore" "negmod" :onemodel-ns)

(in-package :clone-ml)

; seed

(defstruct seed
  (model nil)
  (infomodel nil)
  (item nil)
  (text nil)
  (literals nil)
  (attributes)
  (inim nil))

; hooks

(defun model-import-new-element-hook (name attributes seed)
  (let ((new-seed (make-seed :model (seed-model seed)
                             :infomodel (seed-infomodel seed)
                             :item (seed-item seed)
                             :inim (seed-inim seed))))
    (cond 
     ((eq name 'onemodel-ns:|NegotiationModel|)
      (setf (model-name (seed-model seed)) (cdr (assoc ':|name| attributes)))
      (setf (model-version (seed-model seed)) (cdr (assoc ':|version| attributes))))
     ((eq name ':|informationModel|)
      (setf (im-name (seed-infomodel seed)) (cdr (assoc ':|name| attributes)))
      (setf (seed-inim seed) t)
      (setf (seed-inim new-seed) t)))
     new-seed))


(defun model-import-finish-element-hook (name attributes parent-seed seed)
  (cond 
   ((eq name ':|primitiveType|)
    (let ((type (make-onetype :kind 'primitive)))
      (setf (onetype-name type) (cdr (assoc 'xmi-ns:|type| attributes)))
      (setf (onetype-id type) (cdr (assoc 'xmi-ns:|id| attributes)))
      (push type (model-types (seed-model seed)))))
   ((eq name ':|complexType|)
    (let ((type (make-onetype :kind 'complex)))
      (setf (onetype-name type) (cdr (assoc 'xmi-ns:|type| attributes)))
      (setf (onetype-id type) (cdr (assoc 'xmi-ns:|id| attributes)))
      (push type (model-types (seed-model seed)))))
   ((eq name ':|enumeration|)
    (let ((type (make-onetype :kind 'enum)))
      (setf (onetype-name type) (cdr (assoc 'xmi-ns:|type| attributes)))
      (setf (onetype-id type) (cdr (assoc 'xmi-ns:|id| attributes)))
      (setf (onetype-vals type) (seed-literals seed))
      (if (seed-inim seed)
          (push type (im-types (seed-infomodel seed)))
          (push type (model-types (seed-model seed))))))
   ((eq name ':|literal|)
    (let ((lit (cdr (assoc ':|name| attributes))))
      (push lit (seed-literals parent-seed))))
   ((eq name ':|attribute|)
    (let ((attr (make-item-attribute)))
      (setf (item-attribute-name attr) (cdr (assoc ':|name| attributes)))
      (setf (item-attribute-onetype attr) (cdr (assoc ':|type| attributes)))
      (push attr (seed-attributes parent-seed))))
   ((eq name ':|issue|)
    (let ((issue (make-item-issue)))
      (setf (item-issue-name issue) (cdr (assoc ':|name| attributes)))
      (setf (item-issue-attributes issue) (seed-attributes seed))
      (push issue (item-issues (seed-item parent-seed)))))
   ((eq name ':|item|)
    (progn
      (setf (item-name (seed-item seed)) (cdr (assoc ':|name| attributes)))
      (setf (item-attributes (seed-item seed)) (seed-attributes seed))))
   ((eq name ':|informationModel|)
    (setf (seed-inim parent-seed) nil)))
  parent-seed)

(defun model-import-text-hook (string seed)
  (setf (seed-text seed) string)
  seed)


#|
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
|#

