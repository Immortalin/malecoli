;;;; Created on 2008-04-30 15:58:29

(in-package :clone-ml)


(defun gare-data-pathanme (fn)
  (merge-pathnames
   (make-pathname
    :directory '(:relative "data" "lisp")
    :name fn :type "l" :case :local)
   #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/gare"))

(defun generate-gare-instances ()
  (mlcl-kb:kb-clear mlcl-kbs::gare-instances-kb)
  (load (gare-data-pathanme "service-category"))
  (load (gare-data-pathanme "items"))
  (load (gare-data-pathanme "protos"))
  (load (gare-data-pathanme "concs"))
  (mlcl-kb:kb-save mlcl-kbs::gare-instances-kb)
  nil)

;
; AG functions
;

(defun AG-service-category (code area descr)
  (add-category-instance mlcl-kbs::gare-instances-kb code area descr))

(defmacro AG-item (code data)
  `(add-item-instance mlcl-kbs::gare-instances-kb ,code ',data))

(defmacro AG-conc (code data)
  `(add-conc-instance mlcl-kbs::gare-instances-kb ,code ',data))

(defmacro AG-proto (code data)
  `(add-proto-instance mlcl-kbs::gare-instances-kb ,code ',data))

  
;
; adding functions
;
    
(defun add-category-instance (kb code area descr)
  (let ((ci (kb-make-simple-instance code :kb kb)))
    (instance-add-direct-type ci gare-kb:|gare_category|)
    (frame-add-own-slot-value 
     ci
     gare-kb:|gare_category_code| code)
    (frame-add-own-slot-value
     ci
     gare-kb:|gare_category_business_area| area)
    (frame-add-own-slot-value 
     ci
     gare-kb:|gare_category_description| descr)))

(defun add-agree (kb case-code price)
  (let* ((agree-id (format nil "agreement-gare-~A" case-code))
         (agree (kb-make-simple-instance agree-id :kb kb)))
    (instance-add-direct-type agree (kb-get-cls "gare_proposal" :kb kb))
    (let* ((price-id (format nil "issue-price-gare-~A" case-code))
           (pric (kb-make-simple-instance price-id :kb kb)))
      (instance-add-direct-type pric gare-kb:|gare_issue_price|)
      (frame-add-own-slot-value
       pric
       gare-kb:|gare_price-amount| (float (read-from-string price)))
      (frame-add-own-slot-value
       pric 
       gare-kb:|gare_price-currency| "euro")
      (frame-add-own-slot-value
       agree 
       gare-kb:|proposal-gare_issue_price| pric))
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
 
(defun get-category-instance (kb code)
  (multiple-value-bind (ci new) (kb-get-simple-instance code :kb kb)
    (if new
        (progn 
          (instance-add-direct-type ci gare-kb:|gare_category|)
          (frame-add-own-slot-value 
           ci
           gare-kb:|gare_category_code| code)))
    ci))

(defun get-location-instance (kb comune provincia)
  (let ((location-id (format nil "gare-location-~A-~A" comune provincia)))
    (multiple-value-bind (li new) (kb-get-simple-instance location-id :kb kb) 
      (if new
          (progn
            (instance-add-direct-type li gare-kb:|gare_location|)
            (frame-add-own-slot-value 
             li 
             gare-kb:|gare_location_comune| comune)
            (frame-add-own-slot-value
             li 
             gare-kb:|gare_location_provincia| provincia)))
      li)))

(defun get-public-party-instance (kb comune provincia)
  (let ((location-id (format nil "gare-party-~A-~A" comune provincia)))
    (multiple-value-bind (li new) (kb-get-simple-instance location-id :kb kb)
      (if new
          (progn
            (instance-add-direct-type li gare-kb:|gare_public_party|)
            (frame-add-own-slot-value
             li 
             gare-kb:|gare_party_location|
             (get-location-instance kb comune provincia))))
      li)))

(defun get-gare-party-instance (kb uuid)
  (let ((party-id (format nil "party-gare-~A" uuid)))
    (multiple-value-bind (li new) (kb-get-simple-instance party-id :kb kb)
      (if new
          (progn
            (instance-add-direct-type li gare-kb:|gare_party|)
            (frame-add-own-slot-value
             li
             gare-kb:|gare_party_uuid| uuid)))
      li)))

(defun get-date-instance (datestr kb)
  (declare (ignore kb))
  (let ((date-id (format nil "date-~A" datestr)))
    (multiple-value-bind (di new) (kb-get-simple-instance date-id :kb mlcl-kbs::gare-instances-kb)
      (if new 
          (progn
            (instance-add-direct-type di negotiation-kb:|neg_date|)
            (destructuring-bind (y m d)  (parse-date datestr)
              (frame-add-own-slot-value 
               di negotiation-kb:|date_year| (parse-integer y))
              (frame-add-own-slot-value 
               di negotiation-kb:|date_month| (parse-integer m))
              (frame-add-own-slot-value 
               di negotiation-kb:|date_day| (parse-integer d)))))
      di)))



;
; adding function
;         
          
 
(defun add-item-instance (kb code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (context-id (format nil "gare-case-context-~A" code))
        (item-id (format nil "gare-item-~A" code)))
    (let ((si (kb-make-simple-instance case-id :kb kb))
          (ci (kb-make-simple-instance context-id :kb kb))
          (ii (kb-make-simple-instance item-id :kb kb)))
      (instance-add-direct-type si gare-kb:|gare_case|)
      (instance-add-direct-type ci gare-kb:|gare_context|)
      (instance-add-direct-type ii gare-kb:|gare_item|)
      
      (frame-add-own-slot-value 
       si
       negotiation-kb:|neg_case_context| ci)
      (frame-add-own-slot-value 
       ci
       negotiation-kb:|neg_case_item| ii)
      (frame-add-own-slot-value 
       ii
       gare-kb:|gare_long_description| (nth 0 data))
      (frame-add-own-slot-value
       ii 
       gare-kb:|gare_description| (nth 1 data))
      
      (if (nth 2 data)
          (let ((di (get-date-instance (nth 2 data) kb)))
            (frame-add-own-slot-value
             ii 
             gare-kb:|gare_service_starting_date| di)))
                                                          
      (if (nth 3 data)
          (let ((di (get-date-instance (nth 2 data) kb)))
            (frame-add-own-slot-value 
             ii 
             gare-kb:|gare_service_ending_date| di)))
          
       (if (and (nth 4 data) (nth 5 data))
           (let ((li (get-location-instance kb (nth 4 data) (nth 5 data)))
                 (pai (get-public-party-instance kb (nth 4 data) (nth 5 data))))
             (frame-add-own-slot-value 
              ii 
              gare-kb:|gare_service_location| li)
             (frame-add-own-slot-value 
              ci 
              negotiation-kb:|neg_case_owner| pai)))
      
      (if (nth 6 data)
          (let ((vals nil))
            (dolist (cat (nth 6 data))
              (push (get-category-instance kb cat) vals))
            (frame-add-own-slot-value 
             ii
             gare-kb:|gare_service_categories|
                                       vals)))
      
      )))

(defun add-proto-instance (kb code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (proto-id (format nil "gare-proto-~A" code)))
    (let ((ci (kb-make-simple-instance proto-id :kb kb))
          (casei (kb-get-simple-instance case-id :kb kb)))
      (instance-add-direct-type ci gare-kb:|gare_protocol|)
      (if casei
          (frame-add-own-slot-value
           casei
           negotiation-kb:|neg_case_protocol| ci))
      (if (nth 0 data)
          (let ((di (get-date-instance (nth 0 data) kb)))
            (frame-add-own-slot-value
             ci 
             negotiation-kb:|neg_case_ending_date| di)))
      (if (and (nth 1 data) (not (string= (nth 1 data) "NS")))  
          (frame-add-own-slot-value 
           ci 
           gare-kb:|gare_protocol_laws| (nth 1 data)))
      (if (and (nth 2 data) (not (string= (nth 2 data) "NS")))
          (frame-add-own-slot-value
           ci 
           gare-kb:|gare_protocol_procedure| (nth 2 data)))
      
      (if (nth 4 data)
          (frame-add-own-slot-value
           ci 
           negotiation-kb:|neg_case_starting_price| (float (read-from-string (nth 4 data)))))
     
    
      (frame-add-own-slot-value 
       ci 
       negotiation-kb:|neg_case_model| (kb-get-simple-instance "cbr-neg-gare_Instance_1" :kb kb))
     )))


(defun add-conc-instance (kb code data)
  (let ((case-id (format nil "gare-case-~A" code))
        (conc-id (format nil "gare-conclusion-~A" code)))
    (let ((ci (kb-make-simple-instance conc-id :kb kb))
          (casei (kb-get-cls case-id :kb kb)))
      (instance-add-direct-type ci gare-kb:|gare_conclusion|)      
      
      (if casei
          (frame-add-own-slot-value
           casei
           negotiation-kb:|neg_case_conclusion| ci))
      
      (if (nth 0 data)
          (let ((agree (add-agree kb code (nth 0 data))))
            (frame-add-own-slot-value
             ci
             negotiation-kb:|neg_case_agreement| agree)))
      
      (if (nth 1 data)
          (let ((di (get-date-instance (nth 1 data) kb)))
            (frame-add-own-slot-value
             ci 
             negotiation-kb:|neg_case_conclusion_date| di)))
      
      (if (nth 2 data)
          (let* ((party (get-gare-party-instance kb (nth 2 data))))
            (frame-add-own-slot-value 
             ci
             negotiation-kb:|neg_case_winner| party)))

            )))


