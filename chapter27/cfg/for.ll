define i32 @printstar(i32 %n) {
entry:
   br label %loop

loop:
   %i = phi i32 [ 1, %entry ], [ %nextvar, %loop ]
   %nextvar = add i32 %i, 1

   %cmptmp = icmp ult i32 %i, %n
   %booltmp = zext i1 %cmptmp to i32
   %loopcond = icmp ne i32 %booltmp, 0

   br i1 %loopcond, label %loop, label %afterloop

afterloop:
   ret i32 %i
}
