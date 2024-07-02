[config] :eval

--* Program
--*   Rules Begin
--*     close [force]   open
--*     close [request] wait
--*     wait  [request] wait
--*     wait  [permit]  open
--*     wait  [force]   open
--*   End
--*
--*   Initial state: close
--*   Run [request]
--*   Run [request]
--*   Run [permit]
--* End
Prog (Rules (Rule (EStr "close") (EStr "force") (EStr "open") (Rule (EStr "close") (EStr "request") (EStr "wait") (Rule (EStr "wait") (EStr "request") (EStr "wait") (Rule (EStr "wait") (EStr "permit") (EStr "open") (Rule (EStr "wait") (EStr "force") (EStr "open") (ECon "End" [])))))) (DExp (Run (Run (Run (EStr "close") (EStr "request")) (EStr "request")) (EStr "permit"))))
--:e open
