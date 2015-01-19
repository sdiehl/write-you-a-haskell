define i32 @foo() {
start:
  br i1 true, label %left, label %right
left:
  %plusOne = add i32 0, 1
  br label %merge
right:
  br label %merge
merge:
  %join = phi i32 [ %plusOne, %left], [ -1, %right]
  ret i32 %join
}
