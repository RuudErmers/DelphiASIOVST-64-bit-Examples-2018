unit DAV_StkSphere;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkSphere class.

  This class implements a spherical ball with radius, mass, position, and
  velocity parameters.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkVector3D;

type
  TStkSphere = class
  private
    // Set the radius of the TStkSphere.
    procedure SetRadius(aRadius: Double);

    // Set the mass of the TStkSphere.
    procedure SetMass(aMass: Double);

    // Get the current TStkSphere radius.
    function GetRadius: Double;

    // Get the current TStkSphere mass.
    function GetMass: Double;

  protected
    FPosition      : TStkVector3d;
    FVelocity      : TStkVector3d;
    FWorkingVector : TStkVector3d;
    FRadius        : Double;
    FMass          : Double;
  public
    // Constructor taking an initial radius value.
    constructor Create(const InitialRadius: Double);

    // Class destructor.
    destructor Destroy; override;

    // Set the 3D center position of the TStkSphere.
    procedure SetPosition(anX, aY, aZ: Double);

    // Set the 3D velocity of the TStkSphere.
    procedure SetVelocity(anX, aY, aZ: Double);

    // Get the relative position of the given point to the TStkSphere as a 3D vector.
    function GetRelativePosition(aPosition: TStkVector3d): TStkVector3d;

    // Set the velcoity of the TStkSphere as a 3D vector.
    function GetVelocity(aVelocity: TStkVector3d): Double;

    // Returns the distance from the TStkSphere boundary to the given position (< 0 if inside).
    function IsInside(aPosition: TStkVector3d): Double;

    // Increase the current TStkSphere velocity by the given 3D components.
    procedure AddVelocity(anX, aY, aZ: Double);

    // Move the TStkSphere for the given time increment.
    procedure Tick(TimeIncrement: Double);

    property Position: TStkVector3d read FPosition;
    property Mass: Double read GetMass write SetMass;
    property Radius: Double read GetRadius write SetRadius;
  end;

implementation

uses
  SysUtils;

constructor TStkSphere.Create(const InitialRadius: Double);
begin
  FRadius := InitialRadius;
  FMass := 1.0;
  FPosition := TStkVector3d.Create(0, 0, 0);
  FVelocity := TStkVector3d.Create(0, 0, 0);
end;

destructor TStkSphere.Destroy;
begin
 FreeAndNil(FPosition);
 FreeAndNil(FVelocity);
 inherited;
end;

procedure TStkSphere.SetPosition;
begin
  FPosition.SetXYZ(anX, aY, aZ);
end;

procedure TStkSphere.SetVelocity;
begin
  FVelocity.SetXYZ(anX, aY, aZ);
end;

procedure TStkSphere.SetRadius;
begin
  FRadius := aRadius;
end;

procedure TStkSphere.SetMass;
begin
  FMass := aMass;
end;

function TStkSphere.GetRelativePosition(aPosition: TStkVector3d): TStkVector3d;
begin
  FWorkingVector.SetXYZ(aPosition.getX - FPosition.getX,
    aPosition.getY - FPosition.getY,
    aPosition.getZ - FPosition.getZ);
  Result := FWorkingVector;
end;

function TStkSphere.GetVelocity(aVelocity: TStkVector3d): Double;
begin
  aVelocity.SetXYZ(FVelocity.getX, FVelocity.getY, FVelocity.getZ);
  Result := FVelocity.getLength;
end;

function TStkSphere.IsInside(aPosition: TStkVector3d): Double;
var
  distance: Double;
  tvector: TStkVector3d;
begin
 // Return directed distance from aPosition to spherical boundary ( <
 // 0 if inside).
  tVector := GetRelativePosition(aPosition);
  distance := tVector.getLength;
  Result := distance - FRadius;
end;

function TStkSphere.GetRadius: Double;
begin
  Result := FRadius;
end;

function TStkSphere.GetMass: Double;
begin
  Result := FMass;
end;

procedure TStkSphere.AddVelocity(anX, aY, aZ: Double);
begin
  FVelocity.setX(FVelocity.getX + anX);
  FVelocity.setY(FVelocity.getY + aY);
  FVelocity.setZ(FVelocity.getZ + aZ);
end;

procedure TStkSphere.Tick(TimeIncrement: Double);
begin
  FPosition.setX(FPosition.getX + (TimeIncrement * FVelocity.getX));
  FPosition.setY(FPosition.getY + (TimeIncrement * FVelocity.getY));
  FPosition.setZ(FPosition.getZ + (TimeIncrement * FVelocity.getZ));
end;

end.
