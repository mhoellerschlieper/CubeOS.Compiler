.disp
  move $l2,(0)
  move $l12,(1)
  add r2,6
  move ($l1),r1
  move 5,r2
.l4
  move ($l6),r4
  add r4,1
  move r4,($l6)
  wrt ($l6)
  jsr $l10
  add r2,6
  comp r2,30
  jlt $l4
  jmp $ende
.l10
 move 1,r2
.l11
  add r2,1
  comp r2,10000
  jlt $l11
  ret
.l2
  di
.l7
  add r1,($l3)
  comp r1,31
  jlt $l7
  ei
 ret
.l12
  wst $int
  wrt r5
  wst $text
  wst r4
  wst ' '
  ret
.ende
heap
.l1 data 3
.l3 data 8
.l6 data 1
.int  res 'Interrupt.'
.text res 'Dies ist ein Test.'
.t2 res 'Hallo'
end
