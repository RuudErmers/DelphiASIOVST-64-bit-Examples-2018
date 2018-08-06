unit ArtMain;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, 
  Forms, Controls, Menus, StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, 
  ComCtrls, StdActns, ActnList, ToolWin, ImgList;

type
  TFmArtumes = class(TForm)
    AcEditCopy: TEditCopy;
    AcEditCut: TEditCut;
    AcEditPaste: TEditPaste;
    AcFileClose: TWindowClose;
    AcFileExit: TAction;
    AcFileNew: TAction;
    AcFileOpen: TAction;
    AcFileSave: TAction;
    AcFileSaveAs: TAction;
    AcHelpAbout: TAction;
    ActionList: TActionList;
    AcWindowArrangeAll: TWindowArrange;
    AcWindowCascade: TWindowCascade;
    AcWindowMinimizeAll: TWindowMinimizeAll;
    AcWindowTileHorizontal: TWindowTileHorizontal;
    AcWindowTileVertical: TWindowTileVertical;
    HelpAboutItem: TMenuItem;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MiEdit: TMenuItem;
    MiEditCopy: TMenuItem;
    MiEditCut: TMenuItem;
    MiEditPaste: TMenuItem;
    MiFile: TMenuItem;
    MiFileClose: TMenuItem;
    MiFileExit: TMenuItem;
    MiFileNew: TMenuItem;
    MiFileOpen: TMenuItem;
    MiFileSave: TMenuItem;
    MiFileSaveAs: TMenuItem;
    MiHelp: TMenuItem;
    MiS1: TMenuItem;
    MiWindow: TMenuItem;
    MiWindowArrange: TMenuItem;
    MiWindowCascade: TMenuItem;
    MiWindowMinimizeAll: TMenuItem;
    MiWindowTileHorizontal: TMenuItem;
    MiWindowTileVertical: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    TbCascade: TToolButton;
    TbCopy: TToolButton;
    TbCut: TToolButton;
    TbNew: TToolButton;
    TbOpen: TToolButton;
    TbPaste: TToolButton;
    TbSave: TToolButton;
    TbSeparator1: TToolButton;
    TbSeparator2: TToolButton;
    TbTileHorizontal: TToolButton;
    TbTileVertical: TToolButton;
    ToolBar: TToolBar;
    TbCalculate: TToolButton;
    ToolButton2: TToolButton;
    AcCalculate: TAction;
    procedure AcFileNewExecute(Sender: TObject);
    procedure AcFileOpenExecute(Sender: TObject);
    procedure AcHelpAboutExecute(Sender: TObject);
    procedure AcFileExitExecute(Sender: TObject);
    procedure AcCalculateExecute(Sender: TObject);
  private
    procedure LoadFromFile(const FileName: TFileName);
  end;

var
  FmArtumes: TFmArtumes;

implementation

{$R *.dfm}

uses
  ArtProject, ArtAbout;

procedure TFmArtumes.LoadFromFile(const FileName: TFileName);
var
  Child: TFmProject;
begin
 Child := TFmProject.Create(Application);
 Child.LoadFromFile(FileName);
end;

procedure TFmArtumes.AcFileNewExecute(Sender: TObject);
var
  Child: TFmProject;
begin
 Child := TFmProject.Create(Application);
 Child.Caption := 'Empty Project (' + IntToStr(MDIChildCount) + ')';
end;

procedure TFmArtumes.AcFileOpenExecute(Sender: TObject);
begin
 if OpenDialog.Execute
  then LoadFromFile(OpenDialog.FileName);
end;

procedure TFmArtumes.AcHelpAboutExecute(Sender: TObject);
begin
 FmAbout.ShowModal;
end;

procedure TFmArtumes.AcCalculateExecute(Sender: TObject);
begin
 if ActiveMDIChild is TFmProject then
  begin
   TFmProject(ActiveMDIChild).Calculate;
  end;
end;

procedure TFmArtumes.AcFileExitExecute(Sender: TObject);
begin
 Close;
end;

end.
