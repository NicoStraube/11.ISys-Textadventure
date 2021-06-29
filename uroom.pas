unit URoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs;

type
  TStringArray = array of string;

  TRoom = class(TObject)

  private
    roomId_: integer;
    roomName_, whereAmI_, description_, descriptionEntered_: string;
    north_, east_, south_, west_: TRoom;
    mapPosition_: string;
    isEntered_, hasItem_, requiresKeyCard_: boolean;
    roomImage_, roomItemImage_: TPicture;

    procedure setRoomId(roomId: integer);

    procedure setWhereAmI(whereAmI: string);

  public
    constructor createRoom(roomName: string; roomId: integer);
    procedure setRoomName(roomName: string);
    function getRoomName(): string;

    function getRoomId(): integer;

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

    procedure setHasItem(hasItem: boolean);
    function hasItem(): boolean;

    procedure setRequiresKeyCard(requiresKeyCard: boolean);
    function requiresKeyCard(): boolean;

    procedure setRoomImage(roomImage: TPicture);
    procedure setRoomItemImage(roomItemImage: TPicture);
    function getRoomImage(): TPicture;
    function getRoomItemImage(): TPicture;

  protected

  end;

implementation


constructor TRoom.createRoom(roomName: string; roomId: integer);
begin
  self.setRoomName(roomName);
  self.setRoomId(roomId);
  self.setWhereAmI('Sie befinden sich in: ' + self.getRoomName() + ' \n-----');
  self.setDescriptionEntered('In diesem Raum ist bereits alles erledigt.');
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


// roomId
procedure TRoom.setRoomId(roomId: integer);
begin
  self.roomId_ := roomId;
end;

function TRoom.getRoomId(): integer;
begin
  Result := self.roomId_;
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

// hasItem
procedure TRoom.setHasItem(hasItem: boolean);
begin
  self.hasItem_ := hasItem;
end;

function TRoom.hasItem(): boolean;
begin
  Result := self.hasItem_;
end;

// requiresKeyCard
procedure TRoom.setRequiresKeyCard(requiresKeyCard: boolean);
begin
  self.requiresKeyCard_ := requiresKeyCard;
end;

function TRoom.requiresKeyCard(): boolean;
begin
  Result := requiresKeyCard_;
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
