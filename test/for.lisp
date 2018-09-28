(setq *syntax-table*
    (cons (cons 'for
        (flambda (form)
            (list 'let (list (list (caar form) (cadar form)))
            (list 'while (list '<= (caar form) (caddar form))
            (cadr form)
            (list 'setq (caar form) (list '+ 1 (caar form)))))))
        *syntax-table*))

(for (i 1 10) (println i))
