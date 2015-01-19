define i1 @foo() {
start:
  br label %next
next:
  br label %return
return:
  ret i1 0
}

