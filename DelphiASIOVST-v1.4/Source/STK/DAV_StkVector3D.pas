unit DAV_StkVector3D;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK 3D vector class.

  This class implements a three-dimensional vector.
}

interface

{$I ..\DAV_Compiler.inc}

type
  TStkVector3D = class
  public
    // Default constructor taking optional initial X, Y, and Z values.
    constructor Create(const initX, initY, initZ: Double); reintroduce; virtual;

    // Get the current X value.
    function getX: double;

    // Get the current Y value.
    function getY: double;

    // Get the current Z value.
    function getZ: double;

    // Calculate the vector length.
    function GetLength: Double;

    // Set the X, Y, and Z values simultaniously.
    procedure SetXYZ(anX, aY, aZ: double);

    // Set the X value.
    procedure SetX(const aval: double);

    // Set the Y value.
    procedure SetY(const aval: double);

    // Set the Z value.
    procedure SetZ(const aval: double);

  protected
    myX, myY, myZ: double
  end;

implementation

constructor TStkVector3D.Create;
begin
  myX := initX;
  myY := initY;
  myZ := initZ;
end;

function TStkVector3D.getX;
begin
  Result := myX;
end;

function TStkVector3D.getY;
begin
  Result := myY;
end;

function TStkVector3D.getZ;
begin
  Result := myZ;
end;

function TStkVector3D.getLength;
var
  temp: double;
begin
  temp := myX * myX;
  temp := temp + myY * myY;
  temp := temp + myZ * myZ;
  temp := sqrt(temp);
  Result := temp;
end;

procedure TStkVector3D.setXYZ;
begin
  myX := anX;
  myY := aY;
  myZ := aZ;
end;

procedure TStkVector3D.setX;
begin
  myX := aval;
end;

procedure TStkVector3D.setY;
begin
  myY := aval;
end;

procedure TStkVector3D.setZ;
begin
  myZ := aval;
end;

end.
