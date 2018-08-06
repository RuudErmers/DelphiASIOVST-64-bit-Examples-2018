unit USampleValue;

interface

type      TSampleValue = record
                  private
                      left,right:double;
                      function GetSample(channel:integer):double;
                      procedure SetSample(channel:integer;value:double);
                  public
                      procedure add(value:double);overload;
                      procedure add(value:TSampleValue);overload;
                      procedure zero;
                      procedure one;
                      procedure mul(value:double);overload;
                      procedure mul(value:TSampleValue);overload;
                      procedure bound;
                      property  Sample[channel:integer]:double read GetSample write SetSample; default;
                    end;

implementation

{ TSampleValue }

procedure TSampleValue.add(value: double);
begin
  left:=left+value;
  right:=right+value;
end;

procedure TSampleValue.add(value: TSampleValue);
begin
  left:=left+value.left;
  right:=right+value.right;
end;

procedure TSampleValue.bound;
begin
  if left<-1 then left:=-1;
  if left> 1 then left:= 1;
  if right<-1 then right:=-1;
  if right> 1 then right:= 1;
end;

function TSampleValue.GetSample(channel: integer): double;
begin
  if channel=0 then result:=left else result:=right;
end;

procedure TSampleValue.SetSample(channel: integer; value: double);
begin
  if channel=0 then left:=value else right:=value;
end;

procedure TSampleValue.mul(value: TSampleValue);
begin
  left:=left*value.left;
  right:=right*value.left;
end;

procedure TSampleValue.mul(value: double);
begin
  left:=left*value;
  right:=right*value;
end;

procedure TSampleValue.one;
begin
  left:=1;
  right:=1;
end;

procedure TSampleValue.zero;
begin
  left:=0;
  right:=0;
end;


end.
