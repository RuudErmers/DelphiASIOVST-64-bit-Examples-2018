unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure AddText(sl:Tstringlist;s:string;first:boolean);
    procedure ReplaceText(sl: TStringlist; filename, searchtext: string);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const basepath = 'C:\Users\Ruud\Documents\Projects\Midimania\Components\RMC';

function ThreeStr(n:integer):string;
begin
  result:=inttostr(n);
  if n<10 then result:='00'+result
  else if n<100 then result:='0'+result;
end;

procedure TForm1.AddText(sl:Tstringlist;s:string;first:boolean);
VAR c,t:string;
begin
  t:=sl[sl.Count-1];
  if first then c:='' else c:=',';
  if length(t)+length(s) < 160 then sl[sl.Count-1]:=t+c+s
                               else begin
                                      sl[sl.Count-1]:=t+c;
                                      sl.Add('          '+s);
                                    end;
end;

procedure TForm1.ReplaceText(sl:TStringlist;filename:string;searchtext:string);
VAR sltext:TStringlist;
    i,istart,iend:integer;
begin
  sltext:=TStringList.Create;
  sltext.LoadFromFile(filename);
  istart:=-1;
  iend  :=-1;
  for i:=0 to sltext.Count-1 do
  begin
    if istart=-1 then if pos(' '+searchtext,sltext[i])>0 then istart:=i;
    if iend  =-1 then if pos(' _'+searchtext,sltext[i])>0 then iend:=i;
  end;
  // replace [ istart+1, ieind-1 ]
  for i:=0 to iend-istart-2 do sltext.Delete(istart+1);
  for i:=0 to sl.count-1 do sltext.insert(istart+1,sl[sl.count-1-i]);
  sltext.SaveToFile(filename);
  sltext.free;
end;

procedure TForm1.Button1Click(Sender: TObject);
VAR SearchRec:TSearchRec;
    sl,slEBitmaps,slRMCBitmapNames:TStringList;
    slower,s,sn,filename:string;
    i,w,p:integer;
    first:boolean;
begin
  sl:=TStringList.Create;
  sl.Add('[FILENAMES]');
  sl.Add('Open='+basepath+'\rmc.res');
  sl.Add('Save='+basepath+'\rmc.res');
  sl.Add('Log='+basepath+'\rmc.txt');
  sl.Add('[COMMANDS]');
  sl.Add('//one or more of the following commands ...');
  sl.Add('-delete  ,,');

  slEBitmaps:=TStringList.Create;
  slEBitmaps.Add('type  eBitmaps = (');
  slRMCBitmapNames:=TStringList.Create;
  slRMCBitmapNames.add('const RMCBitmapNames : array[eBitmaps] of string =(');
  w:=FindFirst(basepath+'\resource\*.bmp',$3f,SearchRec);
  first:=true;
  while w=0 do
  begin
    s:=UpperCase(SearchRec.Name);
    p:=pos('.BMP',s);
    if p<>0 then
    begin
      filename:=basepath+'\resource\'+SearchRec.Name;
      sn:=Copy(s,1,p-1);
      slower:=Copy(SearchRec.Name,1,p-1);
      sl.add('-add '+filename+',BITMAP,'+sn+',');
      AddText(slEBitmaps,'Bmp'+slower,first);
      AddText(slRMCBitmapNames,''''+sn+'''',first);
      first:=false;
    end;
    w:=FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  for i:=0 to 100 do
  begin
    filename:=basepath+'\resource\Knob_SlimPlastic\Knob_SlimPlastic_v001_'+ThreeStr(i)+'.png';
    sl.add('-add '+filename+',RCDATA,SP_ROT_'+ThreeStr(i)+',');
  end;
  sl.SaveToFile(basepath+'\script');
  slEBitmaps.Add(');');
  slRMCBitmapNames.Add(');');
  ReplaceText(slEBitmaps,basepath+'\URMCBitmaps.pas','EBITMAPS');
  ReplaceText(slRMCBitmapNames,basepath+'\URMCBitmaps.pas','RMCBITMAPS');
  Button2Click(NIL);
end;

procedure TForm1.Button2Click(Sender: TObject);
VAR exe,cmd:string;
begin
  DeleteFile(basepath+'\rmc.res');
  DeleteFile(basepath+'\rmc.txt');
  CopyFile(basepath+'\resource\rmcstart.res',basepath+'\rmc.res',false);
  exe:='"C:\Program Files (x86)\Resource Hacker\ResourceHacker.exe"';
  cmd:=exe + ' -script '+basepath+'\script';
  WinExec(PAnsiChar(AnsiString(cmd)),SW_NORMAL);
  Sleep(2000);
  try
    Memo1.Lines.LoadFromFile(basepath+'\rmc.txt',TEncoding.Unicode);
  except
    Memo1.Lines.Add(basepath+'\rmc.txt file NOT found ??? ');
  end;

end;

end.
