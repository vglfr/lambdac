; λxy.x v

@0 = private unnamed_addr constant [2 x i8] c"v\00"

; (f, x)
; %T1 = type { i32, i8* }
%T1 = type { i32, [2 x i8] }

declare i32 @puts(i8* nocapture) nounwind

define i32 @main() {
  %1 = getelementptr [2 x i8], [2 x i8]* @0, i64 0, i64 0
  ; call i32 @puts(i8* %1)

  call %T1* @abs1_1(i8* %1)
  ret i32 0
}

define %T1* @abs1_1(i8* %a1) {
  %1 = alloca %T1

  %2 = getelementptr %T1, %T1* %1, i64 0, i32 1
  store [2 x i8] c"u\00", [2 x i8]* %2

  %3 = getelementptr [2 x i8], [2 x i8]* %2, i64 0, i64 0
  call i32 @puts(i8* %3)

  ret %T1* %1
}

; define void @abs1_2(%T1* %a1) {
  ; %1 = getelementptr %T1, %T1* %a1, i64 0, i32 1, i64 0
  ; call i32 @puts(i8* %1)
  ; ret void
; }
