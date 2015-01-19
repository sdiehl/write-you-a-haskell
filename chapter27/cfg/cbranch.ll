define i32 @foo() {
start:
  br i1 true, label %left, label %right
left:
  ret i32 10
right:
  ret i32 20
}
