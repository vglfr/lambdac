; λx.x y

@0 = private unnamed_addr constant [8 x i8] c"λx.x y\00"
@1 = private unnamed_addr constant [2 x i8] c"y\00"

declare i32 @puts(i8* nocapture) nounwind

define i8* @abs1(i8* %a1) {
  ; call i32 @puts(i8* %a1)
  ret i8* %a1
}

define i8* @app1(i8* %a1) {
  ; call i32 @puts(i8* %a1)
  %1 = call i8* @abs1(i8* %a1)
  ret i8* %1
}

define i32 @main() {
  ; %tmp = alloca [8 x i8]
  ; store [8 x i8] c"λx.x y\00", [8 x i8]* %tmp

  %1 = getelementptr [8 x i8], [8 x i8]* @0, i64 0, i64 0
  call i32 @puts(i8* %1)

  %3 = getelementptr [2 x i8], [2 x i8]* @1, i64 0, i64 0
  call i8* @app1(i8* %3)
  ; call i32 @puts(i8* %3)

  ; %5 = getelementptr [8 x i8], [8 x i8]* %tmp, i64 0, i64 0
  ; call i32 @puts(i8* %5)

  ret i32 0
}
