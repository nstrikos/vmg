unit dockedglasssettings;

{$mode objfpc}

interface

uses
  Classes, SysUtils, appsettings;

type

  { TDockedGlassSettings }

  TDockedGlassSettings = class

  private
     mX, mY, mWidth, mHeight : integer;
  public
     constructor Create();
     procedure SetX(X : integer);
     function GetX : integer;
     procedure SetY(Y : integer);
     function GetY : integer;
     procedure SetWidth(Width : integer);
     function GetWidth : integer;
     procedure SetHeight(Height : integer);
     function GetHeight : integer;
     function GetFm : Double;
  end;

var dgSettings : TDockedGlassSettings;

implementation

constructor TDockedGlassSettings.Create;
begin
     mX := vConfigurations.DockedGlassLeft;
     mY := vConfigurations.DockedGlassTop;
     mWidth := vConfigurations.DockedGlassWidth;
     mHeight := vConfigurations.DockedGlassHeight;
end;

procedure TDockedGlassSettings.SetX(x : integer);
begin
  mX := x;
end;

function TDockedGlassSettings.GetX : integer;
begin
    Result := mX;
end;

procedure TDockedGlassSettings.SetY(y : integer);
begin
  mY := y;
end;

function TDockedGlassSettings.GetY : integer;
begin
    Result := mY;
end;

procedure TDockedGlassSettings.SetWidth(Width : integer);
begin
  mWidth := Width;
end;

function TDockedGlassSettings.GetWidth : integer;
begin
    Result := mWidth;
end;

procedure TDockedGlassSettings.SetHeight(Height : integer);
begin
  mHeight := Height;
end;

function TDockedGlassSettings.GetHeight : integer;
begin
    Result := mHeight;
end;

function TDockedGlassSettings.GetFm : Double;
begin
    Result := vConfigurations.iMagnification;
end;


end.

