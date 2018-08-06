unit VSTPropertySheet_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (ueber eine     
// andere Typbibliothek) reimportiert wird oder wenn der Befehl            
// 'Aktualisieren' im Typbibliotheks-Editor waehrend des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Aenderungen gehen verloren.                                        
// ************************************************************************ //

// PASTLWTR : 1.2
// Datei generiert am 19.11.2007 02:10:43 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: C:\Code\VSTPack\Examples\VST Shell Extension\VSTPropertySheet.tlb (1)
// LIBID: {7AD849A6-7662-4436-B0F4-5B84E8825D33}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: Shell extension for displaying VST Plugin information
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typueberpruefung fuer Zeiger compiliert werden. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// In dieser Typbibliothek deklarierte GUIDS . Es werden folgende        
// Praefixe verwendet:                                                     
//   Typbibliotheken     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Nicht-DISP-Interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  VSTPropertySheetMajorVersion = 1;
  VSTPropertySheetMinorVersion = 0;

  LIBID_VSTPropertySheet: TGUID = '{7AD849A6-7662-4436-B0F4-5B84E8825D33}';

  IID_IVSTPluginPropertySheet: TGUID = '{DC4552D5-A08B-47B5-8ECA-20D21DF4F836}';
  CLASS_VSTPluginPropertySheet: TGUID = '{439DF740-4226-49DB-9BD3-CABF3C2CF5DA}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  IVSTPluginPropertySheet = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  VSTPluginPropertySheet = IVSTPluginPropertySheet;


// *********************************************************************//
// Interface: IVSTPluginPropertySheet
// Flags:     (256) OleAutomation
// GUID:      {DC4552D5-A08B-47B5-8ECA-20D21DF4F836}
// *********************************************************************//
  IVSTPluginPropertySheet = interface(IUnknown)
    ['{DC4552D5-A08B-47B5-8ECA-20D21DF4F836}']
  end;

// *********************************************************************//
// Die Klasse CoVSTPluginPropertySheet stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface IVSTPluginPropertySheet, dargestellt 
// von CoClass VSTPluginPropertySheet, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoVSTPluginPropertySheet = class
    class function Create: IVSTPluginPropertySheet;
    class function CreateRemote(const MachineName: string): IVSTPluginPropertySheet;
  end;

implementation

uses ComObj;

class function CoVSTPluginPropertySheet.Create: IVSTPluginPropertySheet;
begin
  Result := CreateComObject(CLASS_VSTPluginPropertySheet) as IVSTPluginPropertySheet;
end;

class function CoVSTPluginPropertySheet.CreateRemote(const MachineName: string): IVSTPluginPropertySheet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_VSTPluginPropertySheet) as IVSTPluginPropertySheet;
end;

end.
