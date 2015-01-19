define i32 @test1(i32 %x, i32 %y, i32 %z) {
  %a = and i32 %z, %x
  %b = and i32 %z, %y
  %c = xor i32 %a, %b
  ret i32 %c
}
