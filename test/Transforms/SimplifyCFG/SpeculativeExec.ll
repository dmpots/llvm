; RUN: opt < %s -simplifycfg -phi-node-folding-threshold=2 -S | FileCheck %s

target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i32 @test1(i32 %a, i32 %b, i32 %c) nounwind  {
; CHECK: @test1
entry:
        %tmp1 = icmp eq i32 %b, 0
        br i1 %tmp1, label %bb1, label %bb3

bb1:            ; preds = %entry
	%tmp2 = icmp sgt i32 %c, 1
	br i1 %tmp2, label %bb2, label %bb3
; CHECK: bb1:
; CHECK-NEXT: icmp sgt i32 %c, 1
; CHECK-NEXT: add i32 %a, 1
; CHECK-NEXT: select i1 %tmp2, i32 %tmp3, i32 %a
; CHECK-NEXT: br label %bb3

bb2:		; preds = bb1
	%tmp3 = add i32 %a, 1
	br label %bb3

bb3:		; preds = %bb2, %entry
	%tmp4 = phi i32 [ %b, %entry ], [ %a, %bb1 ], [ %tmp3, %bb2 ]
        %tmp5 = sub i32 %tmp4, 1
	ret i32 %tmp5
}

declare i8 @llvm.cttz.i8(i8, i1)

define i8 @test2(i8 %a) {
; CHECK: @test2
  br i1 undef, label %bb_true, label %bb_false
bb_true:
  %b = tail call i8 @llvm.cttz.i8(i8 %a, i1 false)
  br label %join
bb_false:
  br label %join
join:
  %c = phi i8 [%b, %bb_true], [%a, %bb_false]
; CHECK: select
  ret i8 %c
}

define i8* @test3(i1* %dummy, i8* %a, i8* %b) {
; Test that a weird, unfolded constant cast in the PHI don't block speculation.
; CHECK: @test3

entry:
  %cond1 = load volatile i1* %dummy
  br i1 %cond1, label %if, label %end

if:
  %cond2 = load volatile i1* %dummy
  br i1 %cond2, label %then, label %end

then:
  br label %end

end:
  %x = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 42 to i8*), %then ]
; CHECK-NOT: phi
; CHECK: select i1 %cond2, i8* inttoptr

  ret i8* %x
}

define i8* @test4(i1* %dummy, i8* %a, i8* %b) {
; Test that we don't speculate an arbitrarily large number of unfolded constant
; expressions.
; CHECK: @test4

entry:
  %cond1 = load volatile i1* %dummy
  br i1 %cond1, label %if, label %end

if:
  %cond2 = load volatile i1* %dummy
  br i1 %cond2, label %then, label %end

then:
  br label %end

end:
  %x1 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 1 to i8*), %then ]
  %x2 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 2 to i8*), %then ]
  %x3 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 3 to i8*), %then ]
  %x4 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 4 to i8*), %then ]
  %x5 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 5 to i8*), %then ]
  %x6 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 6 to i8*), %then ]
  %x7 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 7 to i8*), %then ]
  %x8 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 8 to i8*), %then ]
  %x9 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 9 to i8*), %then ]
  %x10 = phi i8* [ %a, %entry ], [ %b, %if ], [ inttoptr (i64 10 to i8*), %then ]
; CHECK-NOT: select
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*
; CHECK: phi i8*

  ret i8* %x10
}

define i16 @test5(i1* %dummy, i16 %a, i16 %b) {
; Test that we speculate no-op instructions.
; CHECK: @test5

entry:
  %cond1 = load volatile i1* %dummy
  br i1 %cond1, label %if, label %end

if:
  %cond2 = load volatile i1* %dummy
  %a.conv = sext i16 %a to i32
  %b.conv = sext i16 %b to i32
  %cmp = icmp ult i32 %a.conv, %b.conv
  br i1 %cond2, label %then, label %end

then:
  %sub = sub i32 %a.conv, %b.conv
  %sub.conv = trunc i32 %sub to i16
  br label %end

end:
  %x = phi i16 [ %a, %entry ], [ %b, %if ], [ %sub.conv, %then ]
; CHECK-NOT: phi
; CHECK: select i1

  ret i16 %x
}

define i16 @test6(i1* %dummy, i64 %a, i64 %b) {
; Test that we speculate no-op instructions when those instructions are in the
; predecessor but could potentially be sunk.
; CHECK: @test6

entry:
  %cond1 = load volatile i1* %dummy
  %a.conv = trunc i64 %a to i16
  %b.conv = trunc i64 %b to i16
  br i1 %cond1, label %if, label %end

if:
  %cond2 = load volatile i1* %dummy
  %cond3 = load volatile i1* %dummy
  %cond4 = load volatile i1* %dummy
  %cmp = icmp ult i16 %a.conv, %b.conv
  %a.conv2 = trunc i64 %a to i32
  %b.conv2 = trunc i64 %b to i32
  br i1 %cond2, label %then, label %end

then:
  %sub = sub i32 %a.conv2, %b.conv2
  %sub.conv = trunc i32 %sub to i16
  br label %end

end:
  %x = phi i16 [ %a.conv, %entry ], [ %b.conv, %if ], [ %sub.conv, %then ]
; CHECK-NOT: phi
; CHECK: select i1

  ret i16 %x
}

define i16 @test7(i1* %dummy, i16 %a, i16 %b, i32 %x) {
; Test that we don't speculate when there are instructions that could
; potentially sink into the conditional block.
; CHECK: @test7

entry:
  %cond1 = load volatile i1* %dummy
  br i1 %cond1, label %if, label %end

if:
  %cond2 = load volatile i1* %dummy
  %a.conv = sext i16 %a to i32
  %b.conv = sext i16 %b to i32
  %cmp = icmp ult i32 %a.conv, %b.conv
  %a.conv2 = add i32 %a.conv, %x
  br i1 %cond2, label %then, label %end

then:
  %sub = sub i32 %a.conv2, %b.conv
  %sub.conv = trunc i32 %sub to i16
  br label %end

end:
  %y = phi i16 [ %a, %entry ], [ %b, %if ], [ %sub.conv, %then ]
; CHECK-NOT: select
; CHECK: phi i16

  ret i16 %y
}

