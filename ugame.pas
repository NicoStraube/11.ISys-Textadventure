unit UGame;

{

Copyright 2021 Nico Straube

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Grids, URoom;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonReset: TButton;
    buttonAction: TButton;
    buttonStart: TButton;
    buttonEast: TButton;
    buttonSouth: TButton;
    buttonWest: TButton;
    buttonNorth: TButton;
    inventoryItem1Image: TImage;
    inventoryItem2Image: TImage;
    inventoryItem3Image: TImage;
    imageContainer: TImage;
    labelMap: TLabel;
    labelAuthorInfo: TLabel;
    labelInventory3: TLabel;
    labelInventory2: TLabel;
    labelInventory1: TLabel;
    labelInventory: TLabel;
    memo: TMemo;
    stringGrid: TStringGrid;
    procedure buttonActionClick(Sender: TObject);
    procedure buttonEastClick(Sender: TObject);
    procedure buttonNorthClick(Sender: TObject);
    procedure buttonResetClick(Sender: TObject);
    procedure buttonSouthClick(Sender: TObject);
    procedure buttonStartClick(Sender: TObject);
    procedure buttonWestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure changeRoom(toRoom: TRoom);
    function isEntryAllowed(toRoom: TRoom): boolean;
    procedure initGame();
    function isAssetsPresent(): boolean;
    procedure stringGridPrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
  private

  public

  end;

var
  Form1: TForm1;
  // Represents at which point the user is in the game
  step: integer = -2;

  // rooms
  rooms: array [0..5] of TRoom;
  street, reception, guestRoom, bathRoom, pool, diningHall, currentRoom: TRoom;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the rooms
  street := TRoom.createRoom('Straße', 0, '2;4');
  reception := TRoom.createRoom('Rezeption', 1, '2;3');
  guestRoom := TRoom.createRoom('Gästezimmer', 2, '2;2');
  bathRoom := TRoom.createRoom('Badezimmer', 3, '2;1');
  pool := TRoom.createRoom('Pool', 4, '1;3');
  diningHall := TRoom.createRoom('Speisesaal', 5, '3;3');

  // Assign the rooms to an individual place in the array
  rooms[0] := street;
  rooms[1] := reception;
  rooms[2] := guestRoom;
  rooms[3] := bathRoom;
  rooms[4] := pool;
  rooms[5] := diningHall;

  initGame();
end;


// initGame
procedure TForm1.initGame();
begin
  // Customize each room
  street.setAdjoiningRooms(reception, nil, nil, nil);
  street.setDescription(
    'Sie sind nach einer langen Reise an Ihrem Hotel angekommen, nehmen Sie Ihr Gepäck und schauen Sie sich etwas um.');
  street.setDescriptionEntered(
    'Ihr Gepäck wurde von der Straße aufgenommen, Sie wollen sich nun etwas im Hotel umschauen.');
  street.setIsEntered(False);
  street.setHasItem(True);
  street.setRequiresKeyCard(False);

  reception.setAdjoiningRooms(guestRoom, diningHall, street, pool);
  reception.setDescription(
    'Checken Sie ein und nehmen Sie Ihre Schlüsselkarte entgegen.');
  reception.setDescriptionEntered(
    'Sie haben eingecheckt und Ihre Schlüsselkarte bereits erhalten. Sie können sich weiter im Hotel umschauen.');
  reception.setIsEntered(False);
  reception.setHasItem(True);
  reception.setRequiresKeyCard(False);

  guestRoom.setAdjoiningRooms(bathroom, nil, reception, nil);
  guestRoom.setDescription(
    'Sie können nun Ihre Koffer ablegen.');
  guestRoom.setDescriptionEntered(
    'Sie haben Ihr Zimmer nun eingerichtet und wollen sich den Pool anschauen, besorgen Sie sich ein Handtuch.');
  guestRoom.setIsEntered(False);
  guestRoom.setHasItem(False);
  guestRoom.setRequiresKeyCard(True);

  bathRoom.setAdjoiningRooms(nil, nil, guestRoom, nil);
  bathRoom.setDescription(
    'Auf dem Waschbecken liegt ein frisches Handtuch, Sie können es aufnehmen.');
  bathRoom.setDescriptionEntered('Sie haben Ihr Handtuch bereits abgeholt.');
  bathRoom.setIsEntered(False);
  bathRoom.setHasItem(True);
  bathRoom.setRequiresKeyCard(True);

  pool.setAdjoiningRooms(nil, reception, nil, nil);
  pool.setDescription(
    'Der Pool sieht super aus, reservieren Sie eine Liege mit Ihrem Handtuch.');
  pool.setDescriptionEntered(
    'Sie haben Ihren Platz reserviert und können später baden gehen. \nDurch Ihre Anreise fühlen Sie sich nun sehr erschöpft, suchen Sie den Speisesaal auf.');
  pool.setIsEntered(False);
  pool.setHasItem(False);
  pool.setRequiresKeyCard(True);

  diningHall.setAdjoiningRooms(nil, nil, nil, reception);
  diningHall.setDescription(
    'Mhhhm, hier duftet es wunderbar, all das leckere Essen.');
  diningHall.setDescriptionEntered(
    'Das Textadventure ist nun beendet, Sie können sich weiterhin frei bewegen. \nVielen Dank fürs Spielen. ;)');
  diningHall.setIsEntered(False);
  diningHall.setHasItem(False);
  diningHall.setRequiresKeyCard(True);

  // Welcome text
  memo.Lines.Add('Hallo, willkommen im Textadventure.');
  memo.Lines.Add('-----');
  memo.Lines.Add('Bewegen Sie sich, indem Sie die untenstehenden Knöpfe nutzen.');
  memo.Lines.Add('Um zu interagieren, dücken Sie bitte auf "Aktion".');
  memo.Lines.Add('Um zu starten, dücken Sie bitte auf "Start".');

  // Prevent interactions before start
  buttonNorth.Enabled := False;
  buttonEast.Enabled := False;
  buttonSouth.Enabled := False;
  buttonWest.Enabled := False;

  buttonAction.Enabled := False;
  buttonReset.Enabled := False;

  { TODO 1 -oNico Straube : Cache pictures and don't load again and again from file while runtime }
end;

// Responsible for the display of the map
procedure TForm1.stringGridPrepareCanvas(Sender: TObject; aCol, aRow: integer;
  aState: TGridDrawState);
var
  grid: TStringGrid;
  tmpRoom: TRoom;
  indexX, indexY: integer;

begin
  // Cast the object to TStringGrid to use it in the following code
  grid := Sender as TStringGrid;

  // Loop through the rooms and draw the position on the map
  for tmpRoom in rooms do
  begin
    // Split the coordinates
    indexX := StrToInt(tmpRoom.getMapPosition().Split(';')[0]);
    indexY := StrToInt(tmpRoom.getMapPosition().Split(';')[1]);
    if ((aCol = indexX) and (aRow = indexY)) then
    begin
      // Prevent displaying rooms a user can't enter AND check if game has started
      if ((step > -2) and (isEntryAllowed(tmpRoom))) then
        if (tmpRoom.Equals(currentRoom)) then
          // The current room is displayed in green
          grid.Canvas.Brush.Color := clGreen
        else
          grid.canvas.Brush.Color := clGray;
    end;
  end;
end;

// check if assets folder is
function TForm1.isAssetsPresent(): boolean;
begin
  if (DirectoryExists(GetCurrentDir + '\assets\images')) then
    Result := True
  else
    Result := False;
end;

// start
procedure TForm1.buttonStartClick(Sender: TObject);
begin
  // Make sure the \assets\images folder is present to display the pictures
  if (not isAssetsPresent) then
  begin
    ShowMessage('Das Spiel kann nicht gestartet werden, die assets sind nicht vorhanden. '
      + #13#10 +
      'Der Ordner "assets" aus der .zip muss sich im gleichen Verzeichnis wie die .exe befinden.');
    exit;
  end;

  step := -1;

  memo.Lines.Clear;
  buttonStart.Enabled := False;
  buttonAction.Enabled := True;
  buttonReset.Enabled := True;

  // Start with the inital room
  changeRoom(street);
end;

// reset
procedure TForm1.buttonResetClick(Sender: TObject);
begin
  // Clear everything
  step := -2;
  imageContainer.Picture.Assign(nil);

  labelInventory1.Caption := '';
  labelInventory2.Caption := '';
  labelInventory3.Caption := '';

  inventoryItem1Image.Picture.Assign(nil);
  inventoryItem2Image.Picture.Assign(nil);
  inventoryItem3Image.Picture.Assign(nil);

  initGame();
  // Re-draw map
  stringGrid.Refresh;
  buttonStart.Enabled := True;
end;


// isEntryAllowed
function TForm1.isEntryAllowed(toRoom: TRoom): boolean;
begin
  if ((not toRoom.requiresKeyCard()) or (labelInventory2.Caption <> '')) then
    Result := True
  else
    Result := False;
end;

// changeRoom
procedure TForm1.changeRoom(toRoom: TRoom);
begin
  currentRoom := toRoom;
  memo.Lines.Clear;

  // To prevent errors close application if room is null
  if (currentRoom = nil) then
    Close;

  // Make sure the \assets\images folder is present to display the pictures
  if (not isAssetsPresent) then
  begin
    ShowMessage('Das Spiel kann nicht fortgeführt werden, die assets sind nicht vorhanden. '
      + #13#10 +
      'Der Ordner "assets" aus der .zip muss sich im gleichen Verzeichnis wie die .exe befinden.');
    exit;
  end;

  // Print where the user is currently at
  memo.Lines.Add(currentRoom.getWhereAmI().Replace('\n', #13#10));
  stringGrid.Refresh;

  // Check if the user has already interacted with this room
  if (currentRoom.isEntered()) then
  begin
    memo.Lines.Add(currentRoom.getDescriptionEntered().Replace('\n', #13#10));
    imageContainer.Picture.LoadFromFile(GetCurrentDir + '\assets\images\' +
      currentRoom.getRoomName() + '_entered.jpg');
  end
  else
  begin
    memo.Lines.Add(currentRoom.getDescription().Replace('\n', #13#10));
    imageContainer.Picture.LoadFromFile(GetCurrentDir + '\assets\images\' +
      currentRoom.getRoomName() + '_n-entered.jpg');
  end;

  // Get the rooms of each direction and activate or deactivate the button depending on the conditions
  if ((currentRoom.getRoomNorth() <> nil) and
    isEntryAllowed(currentRoom.getRoomNorth())) then
    buttonNorth.Enabled := True
  else
    buttonNorth.Enabled := False;

  if ((currentRoom.getRoomEast() <> nil) and
    isEntryAllowed(currentRoom.getRoomEast())) then
    buttonEast.Enabled := True
  else
    buttonEast.Enabled := False;

  if ((currentRoom.getRoomSouth() <> nil) and
    isEntryAllowed(currentRoom.getRoomSouth())) then
    buttonSouth.Enabled := True
  else
    buttonSouth.Enabled := False;

  if ((currentRoom.getRoomWest() <> nil) and
    isEntryAllowed(currentRoom.getRoomWest())) then
    buttonWest.Enabled := True
  else
    buttonWest.Enabled := False;
end;


// North
procedure TForm1.buttonNorthClick(Sender: TObject);
begin
  changeRoom(currentRoom.getRoomNorth());
end;

// East
procedure TForm1.buttonEastClick(Sender: TObject);
begin
  changeRoom(currentRoom.getRoomEast());
end;

// South
procedure TForm1.buttonSouthClick(Sender: TObject);
begin
  changeRoom(currentRoom.getRoomSouth());
end;

// West
procedure TForm1.buttonWestClick(Sender: TObject);
begin
  changeRoom(currentRoom.getRoomWest());
end;


// action
procedure TForm1.buttonActionClick(Sender: TObject);
begin
  // Loop through the rooms
  case currentRoom.getRoomId() of
    // street:0
    0:
    begin
      if (not currentRoom.isEntered()) then
      begin
        inventoryItem1Image.Picture.LoadFromFile(GetCurrentDir +
          '\assets\images\00luggage.jpg');
        labelInventory1.Caption := 'Gepäck';

        currentRoom.setIsEntered(True);
        Inc(step);
      end;
    end;
    // reception:1
    1:
    begin
      begin
        if (not currentRoom.isEntered()) then
        begin
          inventoryItem2Image.Picture.LoadFromFile(GetCurrentDir +
            '\assets\images\01keycard.jpg');
          labelInventory2.Caption := 'Schlüsselkarte';

          currentRoom.setIsEntered(True);
          Inc(step);
        end;
      end;
    end;
    // guestRoom:2
    2:
    begin
      if (not currentRoom.isEntered()) then
      begin
        if (labelInventory1.Caption <> 'Gepäck') then
        begin
          // Call changeRoom here because the procedure is ended at this point (I dont know why I did that O.o)
          // changeRoom(currentRoom);
          exit;
        end;

        inventoryItem1Image.Picture.Assign(nil);
        labelInventory1.Caption := '';

        currentRoom.setIsEntered(True);
        Inc(step);
      end;
    end;
    // bathRoom:3
    3:
    begin
      if (not currentRoom.isEntered()) then
      begin
        inventoryItem3Image.Picture.LoadFromFile(GetCurrentDir +
          '\assets\images\02towel.jpg');
        labelInventory3.Caption := 'Handtuch';

        currentRoom.setIsEntered(True);
        Inc(step);
      end;
    end;
    // pool:4
    4:
    begin
      if (labelInventory3.Caption <> 'Handtuch') then
        exit;

      inventoryItem3Image.Picture.Assign(nil);
      labelInventory3.Caption := '';

      currentRoom.setIsEntered(True);
      Inc(step);
    end;
    // diningHall:5
    5:
    begin
      if (step <> 4) then
      begin
        ShowMessage('Sie können das Spiel noch nicht beenden, es gibt noch Dinge zu erledigen.');
        exit;
      end;

      buttonAction.Enabled := False;
      currentRoom.setIsEntered(True);
    end;
  end;

  changeRoom(currentRoom);
end;

end.
