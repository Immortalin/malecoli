s;;;; Created on 2008-04-30 15:58:29

(in-package :clone-ml)


(defun gare-data-pathanme (fn)
  (merge-pathnames
   (make-pathname
    :directory '(:relative "data" "lisp")
    :name fn :type "l" :case :local)
   #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/gare/"))

(defun generate-gare-instances ()
  (mlcl-kb:kb-open (mlcl-kb:find-kb 'mlcl-kbs::gare-instances-kb))
  (mlcl-kb:kb-clear (mlcl-kb:find-kb 'mlcl-kbs::gare-instances-kb))
  (let ((mlcl-kb:*kb* (find-kb 'mlcl-kbs::gare-instances-kb)))
    (format t "--> category~%")
    (load (gare-data-pathanme "service-category"))
    (format t "--> items~%")
    (load (gare-data-pathanme "items"))
    (format t "--> protos ~%")
    (load (gare-data-pathanme "protos"))
    (format t "--> concs~%")
    (load (gare-data-pathanme "concs")))
  (format t "--> save all <-- ~%")
  (mlcl-kb:kb-save (mlcl-kb:find-kb 'mlcl-kbs::gare-instances-kb))
  nil)

;
; AG functions
;

(defun AG-service-category (code area descr)
  (add-category-instance code area descr))

(defmacro AG-item (code data)
  `(add-item-instance ,code ',data))

(defmacro AG-conc (code data)
  `(add-conc-instance ,code ',data))

(defmacro AG-proto (code data)
  `(add-proto-instance ,code ',data))

  
;
; adding functions
;
    
(defun add-category-instance (code area descr)
  (let ((ci (make-simple-instance code)))
    (instance-add-direct-type ci 'gare-kb::|gare_category|)
    (setf (frame-own-slot-value ci 'gare-kb::|gare_category_code|) code)
    (setf (frame-own-slot-value ci 'gare-kb::|gare_category_business_area|) area)
    (setf (frame-own-slot-value ci 'gare-kb::|gare_category_description|) descr)))

(defun add-agree (case-code price)
  (let* ((agree-id (format nil "agreement-gare-~A" case-code))
         (agree (make-simple-instance agree-id)))
    (instance-add-direct-type agree (get-cls "gare_proposal"))
    (let* ((price-id (format nil "issue-price-gare-~A" case-code))
           (pric (make-simple-instance price-id)))
      (instance-add-direct-type pric 'gare-kb::|gare_issue_price|)
      (setf (frame-own-slot-value pric 'gare-kb::|gare_price-amount|) (float (read-from-string price)))
      (setf (frame-own-slot-value pric 'gare-kb::|gare_price-currency|) "euro")
      (setf (frame-own-slot-value agree 'gare-kb::|proposal-gare_issue_price|) pric))
    agree))


;
; parsing
;

(defun parse-date (s)
  (let ((pos (position #\- s)))
    (cond
      ((null pos) (list (string-trim " " s)))
      ((= pos 0) (parse-date (subseq s 1 (length s))))
      (t
        (cons (string-trim " " (subseq s 0 pos))
              (parse-date (subseq s (+ pos 1) (length s))))))))


;
; getting functions
;
 
(defun get-category-instance (code)
  (multiple-value-bind (ci new) (get-simple-instance code)
    (if new
        (progn 
          (instance-add-direct-type ci 'gare-kb::|gare_category|)
          (setf (frame-own-slot-value ci 'gare-kb::|gare_category_code|) code)))
    ci))

(defun get-location-instance (comune provincia)
  (let ((location-id (format nil "gare-location-~A-~A" comune provincia)))
    (multiple-value-bind (li new) (get-simple-instance location-id) 
      (if new
          (progn
            (instance-add-direct-type li 'gare-kb::|gare_location|)
            (setf (frame-own-slot-value li 'gare-kb::|gare_location_comune|) comune)
            (setf (frame-own-slot-value li 'gare-kb::|gare_location_provincia|) provincia)))
      li)))

(defun get-public-party-instance (comune provincia)
  (let ((location-id (format nil "gare-party-~A-~A" comune provincia)))
    (multiple-value-bind (li new) (get-simple-instance location-id)
      (if new
          (progn
            (instance-add-direct-type li 'gare-kb::|gare_public_party|)
            (setf (frame-own-slot-value li 'gare-kb::|gare_party_location|)
                  (get-location-instance comune provincia))))
      li)))

(defun get-gare-party-instance (uuid)
  (let ((party-id (format nil "party-gare-~A" uuid)))
    (multiple-value-bind (li new) (get-simple-instance party-id)
      (if new
          (progn
            (instance-add-direct-type li 'gare-kb::|gare_party|)
            (setf (frame-own-slot-value li 'gare-kb::|gare_party_uuid|) uuid)))
      li)))

(defun get-date-instance (datestr)
  (let ((date-id (format nil "date-~A" datestr)))
    (multiple-value-bind (di new) (get-simple-instance date-id)
      (if new 
          (progn
            (instance-add-direct-type di 'dataset-kb::|date|)
            (destructuring-bind (y m d)  (parse-date datestr)
              (setf (frame-own-slot-value di 'dataset-kb::|time_year|) (parse-integer y))
              (setf (frame-own-slot-value di 'dataset-kb::|time_month|) (parse-integer m))
              (setf (frame-own-slot-value di 'dataset-kb::|time_day|) (parse-integer d)))))
      di)))



;
; adding function
;         
          
 
(defun add-item-instance (code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (context-id (format nil "gare-case-context-~A" code))
        (item-id (format nil "gare-item-~A" code)))
    (let ((si (make-simple-instance case-id))
          (ci (make-simple-instance context-id))
          (ii (make-simple-instance item-id)))
      (instance-add-direct-type si 'gare-kb::|gare_case|)
      (instance-add-direct-type ci 'gare-kb::|gare_context|)
      (instance-add-direct-type ii 'gare-kb::|gare_item|)
      
      (setf (frame-own-slot-value si 'negotiation-kb::|neg_case_context|) ci)
      (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_item|) ii)
      (setf (frame-own-slot-value ii 'gare-kb::|gare_long_description|) (nth 0 data))
      (setf (frame-own-slot-value ii 'gare-kb::|gare_description|) (nth 1 data))
      
      (if (nth 2 data)
          (let ((di (get-date-instance (nth 2 data))))
            (setf (frame-own-slot-value ii 'gare-kb::|gare_service_starting_date|) di)))
                                                          
      (if (nth 3 data)
          (let ((di (get-date-instance (nth 2 data))))
            (setf (frame-own-slot-value ii 'gare-kb::|gare_service_ending_date|) di)))
          
       (if (and (nth 4 data) (nth 5 data))
           (let ((li (get-location-instance (nth 4 data) (nth 5 data)))
                 (pai (get-public-party-instance (nth 4 data) (nth 5 data))))
             (setf (frame-own-slot-value ii 'gare-kb::|gare_service_location|) li)
             (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_owner|) pai)))
      
      (if (nth 6 data)
          (let ((vals nil))
            (dolist (cat (nth 6 data))
              (push (get-category-instance cat) vals))
            (setf (frame-own-slot-values ii 'gare-kb::|gare_service_categories|) vals)))
      
      )))

(defun add-proto-instance (code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (proto-id (format nil "gare-proto-~A" code)))
    (let ((ci (make-simple-instance proto-id))
          (casei (get-simple-instance case-id)))
      (instance-add-direct-type ci 'gare-kb::|gare_protocol|)
      (if casei
          (setf (frame-own-slot-value casei 'negotiation-kb::|neg_case_protocol|) ci))
      (if (nth 0 data)
          (let ((di (get-date-instance (nth 0 data))))
            (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_ending_date|) di)))
      (if (and (nth 1 data) (not (string= (nth 1 data) "NS")))  
          (setf (frame-own-slot-value ci 'gare-kb::|gare_protocol_laws|) (nth 1 data)))
      (if (and (nth 2 data) (not (string= (nth 2 data) "NS")))
          (setf (frame-own-slot-value ci 'gare-kb::|gare_protocol_procedure|) (nth 2 data)))
      
      (if (nth 4 data)
          (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_starting_price|) (float (read-from-string (nth 4 data)))))
     
    
      (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_model|) (get-simple-instance "neg-gare-model"))
     )))


(defun add-conc-instance ( code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (conc-id (format nil "gare-conclusion-~A" code)))
    (let ((ci (make-simple-instance conc-id))
          (casei (get-cls case-id)))
      (instance-add-direct-type ci 'gare-kb::|gare_conclusion|)      
      
      (if casei
          (setf (frame-own-slot-value casei 'negotiation-kb::|neg_case_conclusion|) ci))
      
      (if (nth 0 data)
          (let ((agree (add-agree code (nth 0 data))))
            (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_agreement|) agree)))
      
      (if (nth 1 data)
          (let ((di (get-date-instance (nth 1 data))))
            (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_conclusion_date|) di)))
      
      (if (nth 2 data)
          (let* ((party (get-gare-party-instance (nth 2 data))))
            (setf (frame-own-slot-value ci 'negotiation-kb::|neg_case_winner|) party)))
            )))


