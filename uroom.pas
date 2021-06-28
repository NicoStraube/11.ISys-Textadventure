unit URoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs;

type
  TStringArray = array of string;

  TRoom = class(TObject)

  private
    roomName_, whereAmI_, description_, descriptionEntered_: string;
    north_, east_, south_, west_: TRoom;
    mapPosition_: string;
    isEntered_: boolean;
    roomImage_, roomItemImage_: TPicture;

    procedure setWhereAmI(whereAmI: string);

  public
    constructor createRoom(roomName: string);
    procedure setRoomName(roomName: string);
    function getRoomName(): string;

    function getWhereAmI(): string;

    procedure setDescription(description: string);
    function getDescription(): string;

    procedure setDescriptionEntered(descriptionEntered: string);
    function getDescriptionEntered(): string;

    procedure setMapPosition(mapPosition: string);
    function getMapPosition(): string;

    procedure setAdjoiningRooms(north, east, south, west: TRoom);

    function getRoomNorth(): TRoom;
    function getRoomEast(): TRoom;
    function getRoomSouth(): TRoom;
    function getRoomWest(): TRoom;

    procedure setIsEntered(isEntered: boolean);
    function isEntered(): boolean;

    procedure setRoomImage(roomImage: TPicture);
    procedure setRoomItemImage(roomItemImage: TPicture);
    function getRoomImage(): TPicture;
    function getRoomItemImage(): TPicture;

  protected

  end;

implementation


constructor TRoom.createRoom(roomName: string);
begin
  self.roomName_ := roomName;
  self.setWhereAmI('Sie befinden sich in: ' + self.getRoomName() + '.');
  self.setDescriptionEntered('In diesem Raum ist breits alles erledigt.');
  self.setIsEntered(False);
end;


// roomName
procedure TRoom.setRoomName(roomName: string);
begin
  self.roomName_ := roomName;
end;

function TRoom.getRoomName(): string;
begin
  Result := self.roomName_;
end;


// whereAmI
procedure TRoom.setWhereAmI(whereAmI: string);
begin
  self.whereAmI_ := whereAmI;
end;

function TRoom.getWhereAmI(): string;
begin
  Result := self.whereAmI_;
end;


// description
procedure TRoom.setDescription(description: string);
begin
  self.description_ := description;
end;

function TRoom.getDescription(): string;
begin
  Result := self.description_;
end;


// descriptionEntered
procedure TRoom.setDescriptionEntered(descriptionEntered: string);
begin
  self.descriptionEntered_ := descriptionEntered;
end;

function TRoom.getDescriptionEntered(): string;
begin
  Result := self.descriptionEntered_;
end;


// mapPosition
procedure TRoom.setMapPosition(mapPosition: string);
begin
  self.mapPosition_ := mapPosition;
end;

function TRoom.getMapPosition(): string;
begin
  Result := self.mapPosition_;
end;


// adjoiningRooms
procedure TRoom.setAdjoiningRooms(north, east, south, west: TRoom);
begin
  self.north_ := north;
  self.east_ := east;
  self.south_ := south;
  self.west_ := west;
end;

function TRoom.getRoomNorth(): TRoom;
begin
  Result := self.north_;
end;

function TRoom.getRoomEast(): TRoom;
begin
  Result := self.east_;
end;

function TRoom.getRoomSouth(): TRoom;
begin
  Result := self.south_;
end;

function TRoom.getRoomWest(): TRoom;
begin
  Result := self.west_;
end;


// isEntered
procedure TRoom.setIsEntered(isEntered: boolean);
begin
  self.isEntered_ := isEntered;
end;

function TRoom.isEntered(): boolean;
begin
  Result := self.isEntered_;
end;


// roomImage & roomItemImage
procedure TRoom.setRoomImage(roomImage: TPicture);
begin
  self.roomImage_ := roomImage;
end;

procedure TRoom.setRoomItemImage(roomItemImage: TPicture);
begin
  self.roomItemImage_ := roomItemImage;
end;

function TRoom.getRoomImage(): TPicture;
begin
  Result := self.roomImage_;
end;

function TRoom.getRoomItemImage(): TPicture;
begin
  Result := self.roomItemImage_;
end;

end.
