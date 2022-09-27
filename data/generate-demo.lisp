(in-package :pacioli-model)

(let ((journal (make-instance 'journal :name "Demo Journal")))
  (dolist (account '("Assets:Bank" "Assets:Cash" "Equity:Opening Balances"
                     "Income:Salary" "Expenses:Food" "Expenses:Electricity" "Expenses:Gas"))
    (find-or-create-account journal account))
  (let ((tr (make-instance 'transaction :name "Opening Balances" :date (lt:encode-timestamp 0 0 0 0 1 1 2016))))
    (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Bank")
                                            :amount (make-instance 'single-commodity-amount :value 5000 :commodity :EUR)
                                            :reconciled t)
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Cash")
                                            :amount (make-instance 'single-commodity-amount :value 300 :commodity :EUR)
                                            :reconciled t)
                      (make-instance 'entry :account (find-or-create-account journal "Equity:Opening Balances")
                                            :amount (make-instance 'single-commodity-amount :value -5300 :commodity :EUR)))
    (register-transaction journal tr))
  (loop for date = (lt:encode-timestamp 0 0 0 0 23 1 2016) then (lt:timestamp+ date 1 :month)
        for tr = (make-instance 'transaction :name "Salary" :date date)
        while (lt:timestamp< date (lt:today)) do
          (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Bank")
                                            :amount (make-instance 'single-commodity-amount :value 1200 :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month)))
                      (make-instance 'entry :account (find-or-create-account journal "Income:Salary")
                                            :amount (make-instance 'single-commodity-amount :value -1200 :commodity :EUR)))
          (register-transaction journal tr))

  (loop for date = (lt:encode-timestamp 0 0 0 0 23 1 2016) then (lt:timestamp+ date 2 :month)
        for tr = (make-instance 'transaction :name "Electricity" :date date)
        for value = (+ 40 (random 150))
        while (lt:timestamp< date (lt:today)) do
          (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Expenses:Electricity")
                                            :amount (make-instance 'single-commodity-amount :value value :commodity :EUR))
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Bank")
                                            :amount (make-instance 'single-commodity-amount :value (- value) :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month))))
          (register-transaction journal tr))
  
  (loop for date = (lt:encode-timestamp 0 0 0 0 23 1 2016) then (lt:timestamp+ date 4 :month)
        for tr = (make-instance 'transaction :name "Gas" :date date)
        for value = (+ 50 (random 400))
        while (lt:timestamp< date (lt:today)) do
          (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Expenses:Gas")
                                            :amount (make-instance 'single-commodity-amount :value value :commodity :EUR))
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Bank")
                                            :amount (make-instance 'single-commodity-amount :value (- value) :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month))))
          (register-transaction journal tr))

  (loop for date = (lt:encode-timestamp 0 0 0 0 23 1 2016) then (lt:timestamp+ date (+ 2 (random 15)) :day)
        for tr = (make-instance 'transaction :name "Food" :date date)
        for value = (+ 1 (random 200))
        while (lt:timestamp< date (lt:today)) do
          (when (< (value (convert-amount (balance (find-or-create-account journal "Assets:Cash")))) 200)
            (let ((tr (make-instance 'transaction :name "ATM" :date (lt:timestamp- date 2 :day))))
              (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Bank")
                                            :amount (make-instance 'single-commodity-amount :value -200 :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month)))
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Cash")
                                            :amount (make-instance 'single-commodity-amount :value 200 :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month))))
              (register-transaction journal tr)))
          (register-entries* tr
                      (make-instance 'entry :account (find-or-create-account journal "Expenses:Food")
                                            :amount (make-instance 'single-commodity-amount :value value :commodity :EUR))
                      (make-instance 'entry :account (find-or-create-account journal "Assets:Cash")
                                            :amount (make-instance 'single-commodity-amount :value (- value) :commodity :EUR)
                                            :reconciled (lt:timestamp< date (lt:timestamp- (lt:today) 2 :month))))
          (register-transaction journal tr))
          
  (setf (transactions journal) (sort (transactions journal) #'lt:timestamp> :key #'date))
  (export-ledger journal (merge-pathnames "data/demo.ledger"
                                          (asdf:system-source-directory :pacioli))))

