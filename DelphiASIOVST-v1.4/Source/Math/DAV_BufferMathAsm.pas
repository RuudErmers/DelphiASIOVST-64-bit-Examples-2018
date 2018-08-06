unit DAV_BufferMathAsm;

(*

well, I did my best... but ASM is not my world...
--- the-real-myco ---

*)

{$I ..\DAV_Compiler.inc}

interface

uses
  DAV_Types;


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}

{TYPE: TDAVSingleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1, Input2, Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TDAVSingleDynArray; const amount: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVSingleDynArray; const factor1, factor2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVSingleDynArray; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2: single; const summand, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVSingleDynArray; const summand: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2, summand: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2: single; const factor, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVSingleDynArray; const factor: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2, factor: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure GetPeaks(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer); overload;
procedure GetSums(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer); overload;


{TYPE: TDAVArrayOfSingleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TDAVArrayOfSingleDynArray; const amount:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
                    

procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2: single;
                       const summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVArrayOfSingleDynArray; const summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2, summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;    

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2: single;
                       const factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVArrayOfSingleDynArray; const factor: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2, factor: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfSingleDynArray; const factor1, factor2: single; Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure ClearArrays(const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const Input, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

// when Output has no dimensions use this:
procedure CreateArrayCopy(const Input: TDAVArrayOfSingleDynArray; out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer); overload;


{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

{TYPE: TDAVDoubleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1, Input2, Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TDAVDoubleDynArray; const amount: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVDoubleDynArray; const factor1, factor2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVDoubleDynArray; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2: Double; const summand, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVDoubleDynArray; const summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2, summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2: Double; const factor, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVDoubleDynArray; const factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2, factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure GetPeaks(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer); overload;
procedure GetSums(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer); overload;


{TYPE: TDAVArrayOfDoubleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TDAVArrayOfDoubleDynArray; const amount:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2: Double;
                       const summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVArrayOfDoubleDynArray; const summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2, summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2: Double;
                       const factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVArrayOfDoubleDynArray; const factor: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2, factor: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfDoubleDynArray; const factor1, factor2: Double; Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const Input, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

// when Output has no dimensions use this:
procedure CreateArrayCopy(const Input: TDAVArrayOfDoubleDynArray; out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer); overload;

implementation


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}


procedure AddArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [Input1 + ebx].Single  // \
     fadd [Input2 + ebx].Single  //  > Output= Input1+Input2;
     fstp [Output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   Output: TDAVSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [from   + ebx].Single  // \
     fsub [amount + ebx].Single  //  > Output:= from-amount;
     fstp [Output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure MulArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [Input1 + ebx].Single  // \
     fmul [Input2 + ebx].Single  //  > Output:= Input1*Input2;
     fstp [Output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;



procedure AddArrays(const {eax}   Input1: TDAVSingleDynArray;
                    const {stack} Input2: single;
                    const {edx}   Output: TDAVSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [Input1 + dim2].Single  // \
     fadd [Input2       ].Single  //  > Output := Input1 + Input2;
     fstp [Output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;

procedure SubArrays(const {eax}   from: TDAVSingleDynArray;
                    const {stack} amount: single;
                    const {edx}   Output: TDAVSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [from   + dim2].Single  // \
     fsub [amount       ].Single  //  > Output := from - amount;
     fstp [Output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;

procedure MulArrays(const {eax}   Input1: TDAVSingleDynArray;
                    const {stack} Input2: single;
                    const {edx}   Output: TDAVSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [Input1 + dim2].Single  // \
     fmul [Input2       ].Single  //  > Output:= Input1*Input2;
     fstp [Output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2,
                             {ecx}   summand,
                             {stack} Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, Output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2 + ebx].Single  //  > x      = factor1*factor2
     fadd [summand + ebx].Single  //  > Output = x+summand
     fstp [esi + ebx].Single      // /

     jnz @start                   //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TDAVSingleDynArray;
                       const {stack} factor2: single;
                       const {edx}   summand,
                             {ecx}   Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2      ].Single  //  > x      = factor1*factor2
     fadd [summand + ebx].Single  //  > Output = x+summand
     fstp [Output  + ebx].Single  // /

     jnz @start                   //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2: TDAVSingleDynArray;
                       const {stack} summand: single;
                       const {ecx}   Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2 + ebx].Single  //  > x      = factor1*factor2
     fadd [summand      ].Single  //  > Output = x+summand
     fstp [Output  + ebx].Single  // /

     jnz @start                   //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TDAVSingleDynArray;
                       const {stack} factor2,
                             {stack} summand: single;
                       const {edx}   Output: TDAVSingleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, 4                   // dec(dim2);
     fld  [factor1 + dim2].Single  // \
     fmul [factor2       ].Single  //  > x      = factor1*factor2
     fadd [summand       ].Single  //  > Output = x+summand
     fstp [Output  + dim2].Single  // /

     jnz @start                //    loop until dim2 = 0
end;






procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2,
                             {ecx}   factor,
                             {stack} Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, Output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2+ebx].Single  //  > x      = summand1+summand2
     fmul [factor+ebx].Single    //  > Output = x*factor
     fstp [esi+ebx].Single       // /

     jnz @start                  //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TDAVSingleDynArray;
                       const {stack} summand2: single;
                       const {edx}   factor,
                             {ecx}   Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2].Single      //  > x      = summand1+summand2
     fmul [factor+ebx].Single    //  > Output = x*factor
     fstp [Output+ebx].Single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2: TDAVSingleDynArray;
                       const {stack} factor: single;
                       const {ecx}   Output: TDAVSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2+ebx].Single  //  > x      = summand1+summand2
     fmul [factor].Single        //  > Output = x*factor
     fstp [Output+ebx].Single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TDAVSingleDynArray;
                       const {stack} summand2,
                             {stack} factor: single;
                       const {edx}   Output: TDAVSingleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, 4                // dec(dim2);
     fld  [summand1+dim2].Single  // \
     fadd [summand2].Single       //  > x      = summand1*summand2
     fmul [factor].Single         //  > Output = x+factor
     fstp [Output+dim2].Single    // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure AddScaledArrays(const {eax}   Input1,
                                {edx}   Input2: TDAVSingleDynArray;
                          const {stack} factor1,
                                {stack} factor2: single;
                          const {ecx}   Output: TDAVSingleDynArray;
                          const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [Input1+ebx].Single
     fmul [factor1].Single
     fld  [Input2+ebx].Single
     fmul [factor2].Single
     faddp
     fstp [Output+ebx].Single

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;


procedure AddModulatedArrays(const {eax}   Input1,
                                   {edx}   Input2,
                                   {ecx}   envelope1,
                                   {stack} envelope2: TDAVSingleDynArray;
                             const {stack} Output: TDAVSingleDynArray;
                             const {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov edi, envelope2
  mov esi, Output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [Input1+ebx].Single
     fmul [envelope1+ebx].Single
     fld  [Input2+ebx].Single
     fmul [edi+ebx].Single
     faddp
     fstp [esi+ebx].Single

     jnz @start                  //    loop until dim2 = 0

  pop edi
  pop esi
  pop ebx
end;




procedure AddArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fadd [eax+ebx].Single  //  > Output:= Input1+Input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   Output: TDAVArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fsub [eax+ebx].Single  //  > Output:= from-amount;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fmul [eax+ebx].Single  //  > Output:= Input1*Input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;









procedure AddArrays(const {eax}   Input1: TDAVArrayOfSingleDynArray;
                    const {stack} Input2:single;
                    const {edx}   Output: TDAVArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fadd [Input2].Single   //  > Output:= Input1+Input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from: TDAVArrayOfSingleDynArray;
                    const {stack} amount:single;
                    const {edx}   Output: TDAVArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fsub [amount].Single   //  > Output:= from-amount;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   Input1: TDAVArrayOfSingleDynArray;
                    const {stack} Input2:single;
                    const {edx}   Output: TDAVArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fmul [Input2].Single   //  > Output:= Input1*Input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;






procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2: single;
                       const summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TDAVArrayOfSingleDynArray; const summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand, Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2, summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand, Output[i], dim2);
end;






procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2: single; const factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TDAVArrayOfSingleDynArray; const factor: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor, Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2, factor: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor, Output[i], dim2);
end;






procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfSingleDynArray; const factor1, factor2: single; Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddScaledArrays(Input1[i], Input2[i], factor1, factor2, Output[i], dim2);
end;




procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddModulatedArrays(Input1[i], Input2[i], envelope1[i], envelope2[i], Output[i], dim2);
end;



procedure ClearArrays(const {eax} Output: TDAVArrayOfSingleDynArray;
                      const {edx} dim1,
                            {ecx} dim2: integer);
asm
  push ebx
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim2
  mov ebp, eax
  mov eax, 0

  @outerloop:
    sub dim1, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov edi, [ebp+dim1]
    mov ecx, ebx
    rep stosd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop ebx
end;


procedure CopyArrays(const {eax}   Input,
                           {edx}   Output: TDAVArrayOfSingleDynArray;
                     const {ecx}   dim1,
                           {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim1

  @outerloop:
    sub ebx, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov ecx, dim2
    mov esi, [eax+ebx]
    mov edi, [edx+ebx]
    rep movsd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop esi
    pop ebx
end;

procedure CreateArrayCopy(const Input: TDAVArrayOfSingleDynArray; out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  CopyArrays(Input, Output, dim1, dim2);
end;

procedure CreateEmptyArray(out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  ClearArrays(Output, dim1, dim2);
end;     








{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}


procedure AddArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [Input1+ebx].Double  // \
     fadd [Input2+ebx].Double  //  > Output= Input1+Input2;
     fstp [Output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   Output: TDAVDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [from  +ebx].Double  // \
     fsub [amount+ebx].Double  //  > Output:= from-amount;
     fstp [Output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [Input1+ebx].Double  // \
     fmul [Input2+ebx].Double  //  > Output:= Input1*Input2;
     fstp [Output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;






procedure AddArrays(const {eax}   Input1: TDAVDoubleDynArray;
                    const {stack} Input2: Double;
                    const {edx}   Output: TDAVDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [Input1+dim2].Double  // \
     fadd [Input2].Double      //  > Output:= Input1+Input2;
     fstp [Output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure SubArrays(const {eax}   from: TDAVDoubleDynArray;
                    const {stack} amount: Double;
                    const {edx}   Output: TDAVDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [from+dim2].Double  // \
     fsub [amount].Double       //  > Output:= from-amount;
     fstp [Output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure MulArrays(const {eax}   Input1: TDAVDoubleDynArray;
                    const {stack} Input2: Double;
                    const {edx}   Output: TDAVDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [Input1+dim2].Double  // \
     fmul [Input2].Double      //  > Output:= Input1*Input2;
     fstp [Output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;




procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2,
                             {ecx}   summand,
                             {stack} Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, Output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2+ebx].Double  //  > x      = factor1*factor2
     fadd [summand+ebx].Double  //  > Output = x+summand
     fstp [esi+ebx].Double      // /

     jnz @start                //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TDAVDoubleDynArray;
                       const {stack} factor2: Double;
                       const {edx}   summand,
                             {ecx}   Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2].Double      //  > x      = factor1*factor2
     fadd [summand+ebx].Double  //  > Output = x+summand
     fstp [Output+ebx].Double   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2: TDAVDoubleDynArray;
                       const {stack} summand: Double;
                       const {ecx}   Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2+ebx].Double  //  > x      = factor1*factor2
     fadd [summand].Double      //  > Output = x+summand
     fstp [Output+ebx].Double   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TDAVDoubleDynArray;
                       const {stack} factor2,
                             {stack} summand: Double;
                       const {edx}   Output: TDAVDoubleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 3

  @start:
     sub dim2, 8               // dec(dim2);
     fld  [factor1+dim2].Double  // \
     fmul [factor2].Double       //  > x      = factor1*factor2
     fadd [summand].Double       //  > Output = x+summand
     fstp [Output+dim2].Double   // /

     jnz @start                //    loop until dim2 = 0
end;






procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2,
                             {ecx}   factor,
                             {stack} Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, Output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2+ebx].Double  //  > x      = summand1+summand2
     fmul [factor+ebx].Double    //  > Output = x*factor
     fstp [esi+ebx].Double       // /

     jnz @start                  //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TDAVDoubleDynArray;
                       const {stack} summand2: Double;
                       const {edx}   factor,
                             {ecx}   Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2].Double      //  > x      = summand1+summand2
     fmul [factor+ebx].Double    //  > Output = x*factor
     fstp [Output+ebx].Double    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2: TDAVDoubleDynArray;
                       const {stack} factor: Double;
                       const {ecx}   Output: TDAVDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2+ebx].Double  //  > x      = summand1+summand2
     fmul [factor].Double        //  > Output = x*factor
     fstp [Output+ebx].Double    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TDAVDoubleDynArray;
                       const {stack} summand2,
                             {stack} factor: Double;
                       const {edx}   Output: TDAVDoubleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 3

  @start:
     sub dim2, 8                // dec(dim2);
     fld  [summand1+dim2].Double  // \
     fadd [summand2].Double       //  > x      = summand1*summand2
     fmul [factor].Double         //  > Output = x+factor
     fstp [Output+dim2].Double    // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure AddScaledArrays(const {eax}   Input1,
                                {edx}   Input2: TDAVDoubleDynArray;
                          const {stack} factor1,
                                {stack} factor2: Double;
                          const {ecx}   Output: TDAVDoubleDynArray;
                          const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [Input1+ebx].Double
     fmul [factor1].Double
     fld  [Input2+ebx].Double
     fmul [factor2].Double
     faddp
     fstp [Output+ebx].Double

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;


procedure AddModulatedArrays(const {eax}   Input1,
                                   {edx}   Input2,
                                   {ecx}   envelope1,
                                   {stack} envelope2: TDAVDoubleDynArray;
                             const {stack} Output: TDAVDoubleDynArray;
                             const {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov edi, envelope2
  mov esi, Output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [Input1+ebx].Double
     fmul [envelope1+ebx].Double
     fld  [Input2+ebx].Double
     fmul [edi+ebx].Double
     faddp
     fstp [esi+ebx].Double

     jnz @start                  //    loop until dim2 = 0

  pop edi
  pop esi
  pop ebx
end;




procedure AddArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fadd [eax+ebx].Double  //  > Output:= Input1+Input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   Output: TDAVArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fsub [eax+ebx].Double  //  > Output:= from-amount;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   Input1,
                          {edx}   Input2,
                          {ecx}   Output: TDAVArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fmul [eax+ebx].Double  //  > Output:= Input1*Input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;









procedure AddArrays(const {eax}   Input1: TDAVArrayOfDoubleDynArray;
                    const {stack} Input2:Double;
                    const {edx}   Output: TDAVArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fadd [Input2].Double   //  > Output:= Input1+Input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from: TDAVArrayOfDoubleDynArray;
                    const {stack} amount:Double;
                    const {edx}   Output: TDAVArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4                 // dec(dim1);
    jc @cleanup                // if dim1 < 0 then exit;

    mov esi,[eax + ebx]
    mov edi,[edx + ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8               //    dec(dim2);
      fld  [esi + ebx].Double  // \
      fsub [amount   ].Double  //  > Output:= from-amount;
      fstp [edi + ebx].Double  // /

      jnz @innerloop           // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   Input1: TDAVArrayOfDoubleDynArray;
                    const {stack} Input2:Double;
                    const {edx}   Output: TDAVArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4                 // dec(dim1);
    jc @cleanup                // if dim1 < 0 then exit;

    mov esi, [eax + ebx]
    mov edi, [edx + ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8               //    dec(dim2);
      fld  [esi + ebx].Double  // \
      fmul [Input2   ].Double  //  > Output:= Input1*Input2;
      fstp [edi + ebx].Double  // /

      jnz @innerloop           // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;






procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2: Double;
                       const summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TDAVArrayOfDoubleDynArray; const summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand, Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2, summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand, Output[i], dim2);
end;






procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2: Double; const factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TDAVArrayOfDoubleDynArray; const factor: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor, Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2, factor: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor, Output[i], dim2);
end;






procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfDoubleDynArray; const factor1, factor2: Double; Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddScaledArrays(Input1[i], Input2[i], factor1, factor2, Output[i], dim2);
end;




procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddModulatedArrays(Input1[i], Input2[i], envelope1[i], envelope2[i], Output[i], dim2);
end;



procedure ClearArrays(const {eax} Output: TDAVArrayOfDoubleDynArray;
                      const {edx} dim1,
                            {ecx} dim2: integer);
asm
  push ebx
  push edi
  push ebp

  shl dim1, 2
  shl dim2, 1
  mov ebx, dim2
  mov ebp, eax
  mov eax, 0

  @outerloop:
    sub dim1, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov edi, [ebp+dim1]
    mov ecx, ebx
    rep stosd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop ebx
end;


procedure CopyArrays(const {eax}   Input,
                           {edx}   Output: TDAVArrayOfDoubleDynArray;
                     const {ecx}   dim1,
                           {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim1
  shl dim2, 1

  @outerloop:
    sub ebx, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov ecx, dim2
    mov esi, [eax+ebx]
    mov edi, [edx+ebx]
    rep movsd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop esi
    pop ebx
end;

procedure CreateArrayCopy(const Input: TDAVArrayOfDoubleDynArray; out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  CopyArrays(Input, Output, dim1, dim2);
end;

procedure CreateEmptyArray(out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  ClearArrays(Output, dim1, dim2);
end;


procedure GetPeaks(const {eax}   Input: TDAVSingleDynArray;
                     var {edx}   Outputmin,
                         {ecx}   Outputmax: Single;
                   const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi


  mov esi, Input

  shl dim2, 2
  mov ebx, dim2

  mov edi,[Input]
  mov [Outputmin], edi
  mov [Outputmax], edi

  @innerloop:
    sub ebx, 4
    jc @cleanup

    fld [esi+ebx].Single
    fcom [Outputmin].Single
    fnstsw ax
    sahf
    jnb @nostoremin
      fstp st
      fld  [esi+ebx].Single
      fstp [Outputmin].single
      jmp @innerloop

  @nostoremin:
    fcomp [Outputmax].Single
    fnstsw ax
    sahf
    jbe @innerloop
      fld  [esi+ebx].Single
      fstp [Outputmax].single

  jmp @innerloop

  @cleanup:
    pop esi
    pop edi
    pop ebx
{
  push ebx
  push edi
  push esi


  mov esi, Input

  shl dim2, 2
  mov ebx, dim2

  fld [esi].Single
  fld [esi].Single

  @innerloop:
    sub ebx, 4
    jz @cleanup

    fld [esi+ebx].Single
    fcom st(2)
    fnstsw ax
    sahf

    jbe @nostoremin
    fstp st(2)
    jmp @innerloop

    @nostoremin:
      fcom st(1)
      fnstsw ax
      sahf
      jnb @removefromst
        fstp st(1)
        jmp @innerloop

      @removefromst:
        fstp st
        jmp @innerloop

  @cleanup:
    fstp [Outputmin].Single
    fstp [Outputmax].Single
    pop esi
    pop edi
    pop ebx }
end;

procedure GetPeaks(const {eax}   Input: TDAVDoubleDynArray;
                     var {edx}   Outputmin,
                         {ecx}   Outputmax: Double;
                   const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi


  mov esi, Input

  shl dim2, 3
  mov ebx, dim2

  fld [esi].Double
  fld [esi].Double

  @innerloop:
    sub ebx, 8
    jz @cleanup

    fld [esi+ebx].Double
    fcom st(2)
    fnstsw ax
    sahf

    jbe @nostoremin
    fstp st(2)
    jmp @innerloop

    @nostoremin:
      fcom st(1)
      fnstsw ax
      sahf
      jnb @removefromst
        fstp st(1)
        jmp @innerloop

      @removefromst:
        fstp st
        jmp @innerloop

  @cleanup:
    fstp [Outputmin].Double
    fstp [Outputmax].Double
    pop esi
    pop edi
    pop ebx 
end;


procedure GetSums(const {eax}   Input: TDAVSingleDynArray;
                    var {edx}   Outputmin,
                        {ecx}   Outputmax: Single;
                  const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi
  push eax

  mov esi, Input

  shl dim2, 2
  mov ebx, dim2

  fldz
  fldz
  fldz

  @innerloop:
    sub ebx, 4              // dec(dim2);
    jc @cleanup

    fld [esi + ebx].Single
    fcom st(3)
    fnstsw ax
    sahf
    jnb @nostoremin
      faddp st(1),st
      jmp @innerloop

    @nostoremin:
      faddp st(2), st
      jmp @innerloop


  @cleanup:
    fstp [Outputmin].Single
    fstp [Outputmax].Single
    fstp [esi].Single
    pop eax
    pop esi
    pop edi
    pop ebx
end;



procedure GetSums(const {eax}   Input: TDAVDoubleDynArray;
                    var {edx}   Outputmin,
                        {ecx}   Outputmax: Double;
                  const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi
  push eax

  mov esi, Input

  shl dim2, 3
  mov ebx, dim2

  fldz
  fldz
  fldz

  @innerloop:
    sub ebx, 8              // dec(dim2);
    jc @cleanup

    fld [esi + ebx].Double
    fcom st(3)
    fnstsw ax
    sahf
    jnb @nostoremin
      faddp st(1),st
      jmp @innerloop

    @nostoremin:
      faddp st(2), st
      jmp @innerloop

  
  @cleanup:
    fstp [Outputmin].Double
    fstp [Outputmax].Double
    fstp [esi].Double
    pop eax
    pop esi
    pop edi
    pop ebx
end;

procedure GetPeaks(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetPeaks(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetSums(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetSums(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetPeaks(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetPeaks(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetSums(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetSums(Input[i], Outputmin[i], Outputmax[i], dim2);
end;


end.
