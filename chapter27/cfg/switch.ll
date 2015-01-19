define i32 @foo(i32 %a) {
entry:
  switch i32 %a, label %default [ i32 0, label %f
                                  i32 1, label %g
                                  i32 2, label %h ]
f:
  ret i32 1
g:
  ret i32 2
h:
  ret i32 3
default:
  ret i32 0
}
