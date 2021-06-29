unit UGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, URoom;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonAction: TButton;
    buttonStart: TButton;
    buttonEast: TButton;
    buttonSouth: TButton;
    buttonWest: TButton;
    buttonNorth: TButton;
    imageContainer: TImage;
    memo: TMemo;
    procedure buttonActionClick(Sender: TObject);
    procedure buttonEastClick(Sender: TObject);
    procedure buttonNorthClick(Sender: TObject);
    procedure buttonSouthClick(Sender: TObject);
    procedure buttonStartClick(Sender: TObject);
    procedure buttonWestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure changeRoom(toRoom: TRoom);
    // procedure drawInitialMap();
    // procedure updateMap();
    procedure loadImages();
  private

  public

  end;

var
  Form1: TForm1;

  // rooms
  rooms: array [0..5] of TRoom;
  street, reception, guestRoom, bathRoom, pool, diningHall, currentRoom: TRoom;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the rooms
  street := TRoom.createRoom('Straße');
  reception := TRoom.createRoom('Rezeption');
  guestRoom := TRoom.createRoom('Gästezimmer');
  bathRoom := TRoom.createRoom('Badezimmer');
  pool := TRoom.createRoom('Pool');
  diningHall := TRoom.createRoom('Speisesaal');

  // Assign the rooms to an individual place in the array
  rooms[0] := street;
  rooms[1] := reception;
  rooms[2] := guestRoom;
  rooms[3] := bathRoom;
  rooms[4] := pool;
  rooms[5] := diningHall;

  // Customize each room
  street.setAdjoiningRooms(reception, nil, nil, nil);
  street.setDescription(
    'Sie sind nach einer langen Reise an Ihrem Hotel angekommen, nehmen Sie Ihr Gepäck und schauen Sie sich etwas um.');
  street.setDescriptionEntered(
    'Sie haben Ihr Gepäck bereits von der Straße aufgenommen, Sie wollen sich nun etwas im Hotel umschauen.');
  street.setMapPosition('3;5');

  reception.setAdjoiningRooms(guestRoom, diningHall, street, pool);
  reception.setDescription(
    'Checken Sie ein und nehmen Sie Ihre Schlüsselkarte entgegen.');
  reception.setDescriptionEntered(
    'Sie haben eingecheckt und Ihre Schlüsselkarte bereits erhalten. Sie können sich weiter im Hotel umschauen.');
  reception.setMapPosition('3;4');

  guestRoom.setAdjoiningRooms(bathroom, nil, reception, nil);
  guestRoom.setDescription(
    'Sie können nun Ihre Koffer ablegen. \nSie haben Ihr Zimmer nun eingerichtet und wollen sich den Pool anschauen, besorgen Sie sich ein Handtuch.');
  guestRoom.setDescriptionEntered('Hier gibt es nichts mehr zu tun.');
  guestRoom.setMapPosition('3;3');

  bathRoom.setAdjoiningRooms(nil, nil, guestRoom, nil);
  bathRoom.setDescription(
    'Auf dem Waschbecken liegt ein frisches Handtuch, Sie können es aufnehmen.');
  bathRoom.setDescriptionEntered('Sie haben Ihr Handtuch bereits abgeholt.');
  bathRoom.setMapPosition('3;2');

  pool.setAdjoiningRooms(nil, reception, nil, nil);
  pool.setDescription(
    'Der Pool sieht super aus, reservieren Sie eine Liege mit Ihrem Handtuch.');
  pool.setDescriptionEntered(
    'Sie haben Ihren Platz reserviert und können später baden gehen. \nNun verspühren Sie plötzlich ein Grummeln im Magen, Sie sollten den Speisesaal aufsuchen.');
  pool.setMapPosition('2;4');

  diningHall.setAdjoiningRooms(nil, nil, nil, reception);
  diningHall.setDescription(
    'Mhhhm, hier duftet es wunderbar, all das leckere Essen.');
  diningHall.setDescriptionEntered('-');
  diningHall.setMapPosition('4;4');

  // Welcome text
  memo.Lines.Add('Hallo, willkommen im Textadventure.');
  memo.Lines.Add('-----');
  memo.Lines.Add('Bewegen Sie sich, indem Sie die untenstehenden Knöpfe nutzen.');
  memo.Lines.Add('Um zu starten, dücken Sie bitte auf "Start".');

  // Prevent interactions before start
  buttonNorth.Enabled := False;
  buttonEast.Enabled := False;
  buttonSouth.Enabled := False;
  buttonWest.Enabled := False;
  buttonAction.Enabled := False;

  // Load images and store in room-object (maybe implement caching in a future release)
  // loadImages();

  // Draw a map (not required, maybe in a future release)
  // drawInitialMap();
end;


// loadImages
procedure TForm1.loadImages();
var
  tmpRoom: TRoom;
  picture: TPicture;
begin
  picture := TPicture.Create;
  for tmpRoom in rooms do ;
  begin
    try
      picture.LoadFromFile(GetCurrentDir + '\assets\images\' +
        tmpRoom.getRoomName() + '_n-entered.jpg');
      tmpRoom.setRoomItemImage(picture);
    finally
      picture.Free;
    end;

    try
      picture.LoadFromFile(GetCurrentDir + '\assets\images\' +
        tmpRoom.getRoomName() + '_entered.jpg');
      tmpRoom.setRoomImage(picture);
    finally
      picture.Free;
    end;
  end;
end;

// changeRoom
procedure TForm1.changeRoom(toRoom: TRoom);
begin
  currentRoom := toRoom;
  memo.Lines.Clear;

  if (currentRoom = nil) then
    Close;

  memo.Lines.Add(currentRoom.getWhereAmI().Replace('\n', #13#10));

  imageContainer.Picture.CleanupInstance;
  // maybe set size with #picture.bitmap.setSize(x,y)
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
    // currentRoom.setIsEntered(True);
  end;

  // imageContainer.Refresh;

  if (currentRoom.getRoomNorth() <> nil) then
    buttonNorth.Enabled := True
  else
    buttonNorth.Enabled := False;

  if (currentRoom.getRoomEast() <> nil) then
    buttonEast.Enabled := True
  else
    buttonEast.Enabled := False;

  if (currentRoom.getRoomSouth() <> nil) then
    buttonSouth.Enabled := True
  else
    buttonSouth.Enabled := False;

  if (currentRoom.getRoomWest() <> nil) then
    buttonWest.Enabled := True
  else
    buttonWest.Enabled := False;
end;


// start
procedure TForm1.buttonStartClick(Sender: TObject);
begin
  memo.Lines.Clear;
  buttonStart.Enabled := False;
  buttonAction.Enabled := True;

  changeRoom(street);

  { imageContainer.Picture.Bitmap.SetSize(currentRoom.getRoomImage().Width,
    currentRoom.getRoomImage().Height);
  imageContainer.Picture.Bitmap.Canvas.Draw(0, 0, currentRoom.getRoomImage().Bitmap);

  imageContainer.Refresh; }
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

// Action
procedure TForm1.buttonActionClick(Sender: TObject);
begin
  if (not currentRoom.isEntered()) then
    currentRoom.setIsEntered(True);
  changeRoom(currentRoom);
end;

end.
