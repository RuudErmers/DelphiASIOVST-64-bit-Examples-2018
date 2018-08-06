program BMTestDouble;

uses
  Forms,
  BMTestDoubleU in 'BMTestDoubleU.pas' {BufferMathForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBufferMathForm, BufferMathForm);
  Application.Run;
end.
