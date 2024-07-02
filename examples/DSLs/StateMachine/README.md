# StateMachine

This is a sample language from MPS, which describle a state machine and run it.

We implement a simplified version of this language. A sample program is given as follows:
```
Program
  Rules Begin
    close [force]   open
    close [request] wait
    wait  [request] wait
    wait  [permit]  open
    wait  [force]   open
  End

  Initial state: close
  Run [request]
  Run [request]
  Run [permit]
End
```

In this implementation, we pay attention to the rule declaration and match current state and coming event with each rule. The construct `Rules` will declare a varible "rules" scoping on the whole program. Sugar definitions of these constructs can be seen in `StateMachine.sgs`.
