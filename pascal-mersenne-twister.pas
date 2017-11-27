unit mersenne;
{$mode objfpc}{$A8}{$R-}{$Q-}
interface
type
   TMersenne32 = class sealed
   strict private
   const 
     // Define MT19937 constants (32-bit RNG)
     N = 624;M = 397;R = 31;A = $9908B0DF;F = 1812433253;
     U = 11;S = 7;B = $9D2C5680;T = 15;C = $EFC60000;L = 18;
     MASK_LOWER = (QWORD(1) shl R) - 1;
     MASK_UPPER = QWORD(1) shl R;
     class var mt:array[0..N-1] of dword;
     class var index:word;
     class procedure twist;inline;static;
   public
     class constructor create;
     class procedure initialize(const seed:dword);inline;static;
     class function Random:dword;inline;static;
   end;
   
   mer32 = type TMersenne32;

implementation 

class constructor TMersenne32.Create;
begin
  initialize(5489);
end;

class procedure TMersenne32.Initialize(const seed:dword);inline;static;
var 
  i:dword;
begin
  mt[0] := seed;
 for  i := 1 to pred(N) do 
   mt[i] := F * (mt[i - 1] xor (mt[i - 1] shr 30)) + i;
 index := N;
end;

// works around the use of modulo, which is slow on some platforms   
class procedure TMersenne32.Twist;inline;static;
var 
  i:integer;
begin
  for i:=0 to N-M-1 do
    mt[i]:=mt[i+M] xor {twist} (((mt[i] and MASK_UPPER) or 
    (mt[i+1] and MASK_LOWER)) shr 1)xor(dword(-(mt[i+1] and 1)) and A);
  for i:=N-M to N-2 do
    mt[i]:=mt[i+(M-N)]xor{twist}(((mt[i] and MASK_UPPER) or 
    (mt[i+1] and MASK_LOWER)) shr 1)xor(dword(-(mt[i+1] and 1)) and A);
    mt[N-1]:=mt[M-1] xor {twist} (((mt[n-1] and MASK_UPPER) or (mt[0] and 
    MASK_LOWER)) shr 1)xor(dword(-(mt[0] and 1)) and A);
  index:=0;
end;

class function TMersenne32.Random:dword;inline;static;
var
  i:integer;
begin
  i := index;
  if  index >= N then
  begin
    Twist;
    i := index;
  end;
  Result := mt[i];
  index := i + 1;
  Result := Result xor (mt[i] shr U);
  Result := Result xor (Result shl S) and B;
  Result := Result xor (Result shl T) and C;
  Result := Result xor (Result shr L);
end;

end.
