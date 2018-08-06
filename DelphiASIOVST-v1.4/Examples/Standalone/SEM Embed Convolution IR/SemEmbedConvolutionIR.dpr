program SemEmbedConvolutionIR;

{$IFNDEF Registered}
{$R 'DAV_Convolution.res' 'DAV_Convolution.rc'}
{$ELSE}
{$R 'IPP_Convolution.res' 'IPP_Convolution.rc'}
{$ENDIF}

uses
  Forms,
  ECImain in 'ECImain.pas' {FmSemEmbedConvolutionIR};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSemEmbedConvolutionIR, FmSemEmbedConvolutionIR);
  Application.Run;
end.
