program BMTestSingle;

uses
  Forms,
  BMTestSingleU in 'BMTestSingleU.pas' {BufferMathForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBufferMathForm, BufferMathForm);
  Application.Run;
end.
