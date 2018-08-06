unit BlowfishMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_CryptBlowfish;

type
  TFmBlowfish = class(TForm)
    EdInputFile  : TEdit;
    RbDecryption : TRadioButton;
    LbInputFile  : TLabel;
    EdOutputFile : TEdit;
    LbOutputFile : TLabel;
    RbEncryption : TRadioButton;
    BtInputFile  : TButton;
    BtOutputFile : TButton;
    LbDirection  : TLabel;
    BtExecute    : TButton;
    OD           : TOpenDialog;
    LbPassword: TLabel;
    EdPassword: TEdit;
    procedure BtExecuteClick(Sender: TObject);
    procedure BtInputFileClick(Sender: TObject);
    procedure BtOutputFileClick(Sender: TObject);
    procedure LbInputFileClick(Sender: TObject);
  private
    procedure CheckCanExecute;
    procedure TestBlowfish;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmBlowfish: TFmBlowfish;

implementation

{$R *.dfm}

procedure TFmBlowfish.BtExecuteClick(Sender: TObject);
var
  FS  : TFileStream;
begin
 if RbEncryption.Checked then
  begin
   // check if file exists
   if not FileExists(EdInputFile.Text)
    then Exit;

   // copy file
   CopyFile(PChar(EdInputFile.Text), PChar(EdOutputFile.Text), False);

   // encrypt stream
   FS := TFileStream.Create(EdInputFile.Text, fmOpenReadWrite);
   with TBlowFish.Create(EdPassword.Text) do
    try
     Encrypt(FS);
    finally
     Free;
    end;
    
  end
 else
  begin

  end;
end;

procedure TFmBlowfish.BtInputFileClick(Sender: TObject);
begin
 if OD.Execute then
  begin
   EdInputFile.Text := OD.FileName;
   if (EdOutputFile.Text = '') and RbEncryption.Checked
    then EdOutputFile.Text := EdInputFile.Text + '.blo';
   CheckCanExecute;
  end;
end;

procedure TFmBlowfish.BtOutputFileClick(Sender: TObject);
begin
 if OD.Execute then
  begin
   EdOutputFile.Text := OD.FileName;
   if (EdInputFile.Text = '') and FileExists(EdOutputFile.Text)
    then EdInputFile.Text := EdOutputFile.Text;
   CheckCanExecute;
  end;
end;

procedure TFmBlowfish.CheckCanExecute;
begin
 BtExecute.Enabled := FileExists(EdInputFile.Text) and
   (EdOutputFile.Text <> ''); 
end;

procedure TFmBlowfish.LbInputFileClick(Sender: TObject);
begin
 TestBlowfish;
end;

procedure TFmBlowfish.TestBlowfish;
var
  i, j : Integer;
  Data : array [0..1] of Cardinal;
  MS   : TMemoryStream;
begin
 MS := TMemoryStream.Create;
 try
  with TBlowFish.Create(EdPassword.Text) do
   try
    // simple test
    Data[0] := 0;
    Data[1] := 1;
    BlockEncrypt(Data[0], Data[1]);
    BlockDecrypt(Data[0], Data[1]);
    Assert(Data[0] = 0);
    Assert(Data[1] = 1);

    // build test stream
    for i := 0 to 1024 - 1
     do MS.Write(i, 4);

    // Encrypt Stream
    Encrypt(MS);

    // Decrypt Stream
    Decrypt(MS);

    // reset position
    MS.Seek(0, soFromBeginning);

    // build test stream
    for i := 0 to 1024 - 1 do
     begin
      MS.Read(j, 4);
      Assert(i = j);
     end;
   finally
    Free;
   end;
 finally
  FreeAndNil(MS);
 end;
end;

end.
