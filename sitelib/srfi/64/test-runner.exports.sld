(export
 ;; The data type
 test-runner-null test-runner? test-runner-reset

 test-result-alist test-result-alist!

 test-runner-pass-count test-runner-pass-count!
 test-runner-fail-count test-runner-fail-count!
 test-runner-xpass-count test-runner-xpass-count!
 test-runner-xfail-count test-runner-xfail-count!
 test-runner-skip-count test-runner-skip-count!
 %test-runner-total-count %test-runner-total-count!

 %test-runner-count-list %test-runner-count-list!

 %test-runner-run-list %test-runner-run-list!

 %test-runner-skip-list %test-runner-skip-list!
 %test-runner-fail-list %test-runner-fail-list!

 %test-runner-skip-save %test-runner-skip-save!
 %test-runner-fail-save %test-runner-fail-save!

 test-runner-group-stack test-runner-group-stack!
 test-runner-group-path

 test-runner-on-test-begin test-runner-on-test-begin!
 test-runner-on-test-end test-runner-on-test-end!
 test-runner-on-group-begin test-runner-on-group-begin!
 test-runner-on-group-end test-runner-on-group-end!
 test-runner-on-final test-runner-on-final!
 test-runner-on-bad-count test-runner-on-bad-count!
 test-runner-on-bad-end-name test-runner-on-bad-end-name!

 %test-runner-on-bad-error-type %test-runner-on-bad-error-type!

 test-runner-aux-value test-runner-aux-value!

 %test-runner-log-file %test-runner-log-file!
 %test-runner-log-port %test-runner-log-port!

 ;; State
 test-result-ref test-result-set!
 test-result-remove test-result-clear
 test-runner-test-name test-result-kind test-passed?

 ;; Factory and current instance
 test-runner-factory test-runner-create
 test-runner-current test-runner-get
 )
