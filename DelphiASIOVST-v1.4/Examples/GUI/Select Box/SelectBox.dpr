program SelectBox;

uses
  Forms,
  SelectBoxTest in 'SelectBoxTest.pas' {FmSelectBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSelectBox, FmSelectBox);
  Application.Run;
end.
