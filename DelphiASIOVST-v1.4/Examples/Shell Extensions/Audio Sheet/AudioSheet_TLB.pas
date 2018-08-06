unit AudioSheet_TLB;

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
// Datei generiert am 25.09.2007 16:00:30 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: C:\Code\ShellExtensions\Audio Sheet\Source\AudioSheet.tlb (1)
// LIBID: {048042D1-7AEE-4253-810D-A3B38DD29335}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: Shell extension for displaying Audio information
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Fehler
//   Hinweis: AudioSheet geändert zu AudioSheet_
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
  AudioSheetMajorVersion = 1;
  AudioSheetMinorVersion = 0;

  LIBID_AudioSheet: TGUID = '{048042D1-7AEE-4253-810D-A3B38DD29335}';

  IID_IAudioSheet: TGUID = '{6E78664C-E0CC-40EF-942A-3D281DF41367}';
  CLASS_AudioSheet_: TGUID = '{553A70D6-4D56-4CDC-8162-76622B82D2B7}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  IAudioSheet = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  AudioSheet_ = IAudioSheet;


// *********************************************************************//
// Interface: IAudioSheet
// Flags:     (256) OleAutomation
// GUID:      {6E78664C-E0CC-40EF-942A-3D281DF41367}
// *********************************************************************//
  IAudioSheet = interface(IUnknown)
    ['{6E78664C-E0CC-40EF-942A-3D281DF41367}']
  end;

// *********************************************************************//
// Die Klasse CoAudioSheet_ stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface IAudioSheet, dargestellt 
// von CoClass AudioSheet_, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoAudioSheet_ = class
    class function Create: IAudioSheet;
    class function CreateRemote(const MachineName: string): IAudioSheet;
  end;

implementation

uses ComObj;

class function CoAudioSheet_.Create: IAudioSheet;
begin
  Result := CreateComObject(CLASS_AudioSheet_) as IAudioSheet;
end;

class function CoAudioSheet_.CreateRemote(const MachineName: string): IAudioSheet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AudioSheet_) as IAudioSheet;
end;

end.
