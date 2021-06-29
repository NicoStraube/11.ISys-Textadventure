unit UGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, URoom;

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
    Label1: TLabel;
    labelInventory3: TLabel;
    labelInventory2: TLabel;
    labelInventory1: TLabel;
    labelInventory: TLabel;
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
  private

  public

  end;

var
  Form1: TForm1;
  // Represents at which point the user is in the game
  step: integer = -1;

  // rooms
  rooms: array [0..5] of TRoom;
  street, reception, guestRoom, bathRoom, pool, diningHall, currentRoom: TRoom;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the rooms
  street := TRoom.createRoom('Straße', 0);
  reception := TRoom.createRoom('Rezeption', 1);
  guestRoom := TRoom.createRoom('Gästezimmer', 2);
  bathRoom := TRoom.createRoom('Badezimmer', 3);
  pool := TRoom.createRoom('Pool', 4);
  diningHall := TRoom.createRoom('Speisesaal', 5);

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
    'Ihr Gepäck wurde von der Straße aufgenommen, Sie wollen sich nun etwas im Hotel umschauen.');
  street.setMapPosition('3;5');
  street.setHasItem(True);
  street.setRequiresKeyCard(False);

  reception.setAdjoiningRooms(guestRoom, diningHall, street, pool);
  reception.setDescription(
    'Checken Sie ein und nehmen Sie Ihre Schlüsselkarte entgegen.');
  reception.setDescriptionEntered(
    'Sie haben eingecheckt und Ihre Schlüsselkarte bereits erhalten. Sie können sich weiter im Hotel umschauen.');
  reception.setMapPosition('3;4');
  reception.setHasItem(True);
  reception.setRequiresKeyCard(False);

  guestRoom.setAdjoiningRooms(bathroom, nil, reception, nil);
  guestRoom.setDescription(
    'Sie können nun Ihre Koffer ablegen.');
  guestRoom.setDescriptionEntered(
    'Sie haben Ihr Zimmer nun eingerichtet und wollen sich den Pool anschauen, besorgen Sie sich ein Handtuch.');
  guestRoom.setMapPosition('3;3');
  guestRoom.setHasItem(False);
  guestRoom.setRequiresKeyCard(True);

  bathRoom.setAdjoiningRooms(nil, nil, guestRoom, nil);
  bathRoom.setDescription(
    'Auf dem Waschbecken liegt ein frisches Handtuch, Sie können es aufnehmen.');
  bathRoom.setDescriptionEntered('Sie haben Ihr Handtuch bereits abgeholt.');
  bathRoom.setMapPosition('3;2');
  bathRoom.setHasItem(True);
  bathROom.setRequiresKeyCard(True);

  pool.setAdjoiningRooms(nil, reception, nil, nil);
  pool.setDescription(
    'Der Pool sieht super aus, reservieren Sie eine Liege mit Ihrem Handtuch.');
  pool.setDescriptionEntered(
    'Sie haben Ihren Platz reserviert und können später baden gehen. \nDurch Ihre Anreise fühlen Sie sich nun sehr erschöpft, suchen Sie den Speisesaal auf.');
  pool.setMapPosition('2;4');
  pool.setHasItem(False);
  pool.setRequiresKeyCard(True);

  diningHall.setAdjoiningRooms(nil, nil, nil, reception);
  diningHall.setDescription(
    'Mhhhm, hier duftet es wunderbar, all das leckere Essen.');
  diningHall.setDescriptionEntered(
    'Das Textadventure ist nun beendet, Sie können sich weiterhin frei bewegen. \nVielen Dank fürs Spielen. :)');
  diningHall.setMapPosition('4;4');
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

  // Load images and store in room-object (maybe implement caching in a future release)

  // Draw a map (not required, maybe in a future release)
  // drawInitialMap();
end;


// changeRoom
procedure TForm1.changeRoom(toRoom: TRoom);
begin
  currentRoom := toRoom;
  memo.Lines.Clear;

  if (currentRoom = nil) then
    Close;

  memo.Lines.Add(currentRoom.getWhereAmI().Replace('\n', #13#10));

  imageContainer.Picture.Assign(nil);
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
  // buttonReset.Enabled := True;

  changeRoom(street);
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
          // Call changeRoom here because the procedure is ended at this point (I dont know, why I did that O.o)
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
