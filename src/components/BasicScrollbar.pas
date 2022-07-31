unit BasicScrollbar;

{$mode objfpc}{$H+}

interface

(* TBasicScrollbar is a vertical scrollbar made with standard controls (TPanel, TShape,
TSpeedButton) to replace Windows native scrollbar that cannot be customized at all.

Its goal is to be fast, simple & easy.

It is possible to set a theme (see TBasicScrollbarTheme) with 3 states :
- NoScroll : when the scrollbar is disabledn (= thumb invisible)
- Active : when the scrollbar is enabled (thumb is visible) and the mouse is over
- Inactive : the scrollbar is enabled and the mouse is not over it.
*)

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  StdCtrls, ExtCtrls, Buttons;

type

TBasicScrollbarColorSetRec = record
    Track:TColor;
    Thumb:TColor;
    Buttons:TColor;
end;

TBasicScrollbarThemeRec = record
    NoScroll:TBasicScrollbarColorSetRec; // scrollbar disabled (= thumb not visible)
    Inactive:TBasicScrollbarColorSetRec; // thumb visible, mouse not over it
    Active:TBasicScrollbarColorSetRec; // thumb visible, mouse over it
end;

{ TBasicScrollbarColorSet }

TBasicScrollbarColorSet = class(TPersistent)
  protected
    FTrack:TColor; // track color
    FThumb:TColor; // thumb color
    FButtons:TColor; // up & down buttons caption
  public
    procedure Assign(Source:TPersistent); override;
    procedure LoadFromRec(rec:TBasicScrollbarColorSetRec);
  published
    property Track: TColor read FTrack write FTrack;
    property Thumb: TColor read FThumb write FThumb;
    property Buttons: TColor read FButtons write FButtons;
end;

{ TBasicScrollbarTheme }

TBasicScrollbarTheme = class(TPersistent)
  protected
    FNoScroll:TBasicScrollbarColorSet;
    FInactive: TBasicScrollbarColorSet;
    FActive: TBasicScrollbarColorSet;
    FOnChange:TNotifyEvent;
    procedure SetNoScroll(Value: TBasicScrollbarColorSet);
    procedure SetInactive(Value: TBasicScrollbarColorSet);
    procedure SetActive(Value: TBasicScrollbarColorSet);
    procedure Changed;
  public
    procedure Assign(Source:TPersistent); override;
    procedure LoadFromRec(rec: TBasicScrollbarThemeRec);
    constructor Create();
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property NoScroll: TBasicScrollbarColorSet read FNoScroll write SetNoScroll;
    property Inactive: TBasicScrollbarColorSet read FInactive write SetInactive;
    property Active: TbasicScrollbarColorSet read FActive write SetActive;
end;

TBasicScrollbarEventType = (
BS_NONE,
BS_PAGE_UP,
BS_PAGE_DOWN,
BS_LINE_UP,
BS_LINE_DOWN,
BS_WHEEL_UP,
BS_WHEEL_DOWN,
BS_THUMB_MOVE
);

TBasicScrollbarEvent = procedure(Sender:TObject; Event:TBasicScrollbarEventType) of object;

TBasicScrollbar = class(TPanel)
private
  _CurrColorSet:TBasicScrollbarColorSet;
protected
  FEnabled:boolean;
  FUpButton:TSpeedButton;
  FDownButton:TSpeedButton;
  FTrackPanel:TPanel;
  FThumbShape:TShape;
  FTimer:TTimer;
  FTheme: TBasicScrollbarTheme;
  FThinnerInactiveThumb:boolean;
  FOnScrollEvent:TBasicScrollbarEvent;
  procedure OnThemeChanged(Sender:TObject);
  procedure SetThinnerInactiveThumb(Value: boolean);
  procedure SetThemeRec(AThemeRec: TBasicScrollbarThemeRec);
  procedure ApplyColorSet(AColorSet:TBasicScrollbarColorSet);
  procedure ApplyTheme();
  procedure Init();
  procedure SetTheme(ATheme: TBasicScrollbarTheme);
  procedure SetEnabled(val:boolean); override;
  procedure OnTimer(Sender:TObject);
  procedure OnMouseUpDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  procedure WhenMouseEnter(Sender:TObject);
  procedure WhenMouseLeave(Sender:TObject);
  procedure OnThumbMouseUpDown(Sender:TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  procedure ScrollEvent(Event:TBasicScrollbarEventType); virtual;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
published
  // make them visible in the IDE
  property Theme: TBasicScrollbarTheme read FTheme write SetTheme;
  property UpButton:TSpeedButton read FUpButton;
  property DownButton:TSpeedButton read FDownButton;
  property TrackPanel:TPanel read FTrackPanel;
  property ThumbShape: TShape read FThumbShape;
  property Enabled:boolean read FEnabled write SetEnabled;
  property ThinnerInactiveThumb:boolean read FThinnerInactiveThumb write SetThinnerInactiveThumb;
  property OnScrollEvent:TBasicScrollbarEvent read FOnScrollEvent write FOnScrollEvent;
end;

procedure Register;

// Themes provided as examples. Make your own theme and set Theme property
const
  BASICSCROLLBAR_LIGHT_THEME: TBasicScrollbarThemeRec = (
  	NoScroll:(Track: clMenuBar; Thumb: clBtnFace; Buttons:clScrollbar);
  	Inactive:(Track: clMenuBar; Thumb: clScrollBar; Buttons:clBtnShadow);
  	Active:(Track: clMenuBar; Thumb: clActiveBorder;Buttons:clBtnShadow);
      );
  BASICSCROLLBAR_DARK_THEME: TBasicScrollbarThemeRec = (
  	NoScroll:(Track: clBlack; Thumb: clBlack; Buttons:clBlack);
  	Inactive:(Track: clBlack; Thumb: $00777777; Buttons:$00222222);
  	Active:(Track: $00222222; Thumb: $00777777; Buttons:$00777777);
      );



implementation

uses
  LCLIntf, LCLType, Math;

procedure Register;
begin
  RegisterComponents('Air16 Controls',[TBasicScrollbar]);
end;

{ TBasicScrollbarColorSet }

procedure TBasicScrollbarColorSet.Assign(Source: TPersistent);
var cs:TBasicScrollbarColorSet;
begin
  if Source is TBasicScrollbarColorSet then begin
      cs := TBasicScrollbarColorSet(Source);
	  FButtons:=cs.Buttons;
	  FThumb:=cs.Thumb;
	  FTrack:=cs.Track;
  end
  else
	inherited Assign(Source);
end;

procedure TBasicScrollbarColorSet.LoadFromRec(rec: TBasicScrollbarColorSetRec);
begin
    FThumb := rec.Thumb;
    FTrack := rec.Track;
    FButtons := rec.Buttons;
end;

{ TBasicScrollbarTheme }

constructor TBasicScrollbarTheme.Create;
begin
    FNoScroll := TBasicScrollbarColorSet.Create;
    FInactive := TBasicScrollbarColorSet.Create;
    Factive := TBasicScrollbarColorSet.Create;
    inherited Create();
end;

procedure TBasicScrollbarTheme.Assign(Source: TPersistent);
var th:TBasicScrollbarTheme;
begin
  if Source is TBasicScrollbarTheme then begin
	th := TBasicScrollbarTheme(Source);
    FActive.Assign(th.Active);
    FInactive.Assign(th.Inactive);
    FNoScroll.Assign(th.NoScroll);
  end
  else
	inherited Assign(Source);
end;

procedure TBasicScrollbarTheme.Changed;
begin
    if Assigned(FOnChange) then
        FOnChange(self);
end;

destructor TBasicScrollbarTheme.Destroy;
begin
  FNoScroll.Free;
  FInactive.Free;
  FActive.Free;
	inherited Destroy;
end;

procedure TBasicScrollbarTheme.LoadFromRec(rec: TBasicScrollbarThemeRec);
begin
	FNoScroll.LoadFromRec(rec.NoScroll);
    FInactive.LoadFromRec(rec.Inactive);
    FActive.LoadFromRec(rec.Active);
    Changed;
end;

procedure TBasicScrollbarTheme.SetActive(Value: TBasicScrollbarColorSet);
begin
	if Value = FActive then exit;
	FActive.Assign(Value);
    Changed;
end;

procedure TBasicScrollbarTheme.SetInactive(Value: TBasicScrollbarColorSet);
begin
	if Value = FInActive then exit;
	FInActive.Assign(Value);
    Changed;
end;

procedure TBasicScrollbarTheme.SetNoScroll(Value: TBasicScrollbarColorSet);
begin
	if Value = FNoScroll then exit;
	FNoScroll.Assign(Value);
    Changed;
end;

{ TBasicScrollbar }

constructor TBasicScrollbar.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

    FTheme := TBasicScrollbarTheme.Create;
    FTheme.OnChange:=@OnThemeChanged;
    _currColorSet := TBasicScrollbarColorSet.Create;
    //AutoSize := false;

    FTimer := TTimer.Create(self);
    FTimer.Enabled:=false;

    FUpButton := TSpeedButton.create(self);
    FDownButton := TSpeedButton.create(self);
    FTrackPanel := TPanel.Create(self);
    FThumbShape := TShape.create(self);

    FUpButton.Parent := self;
    FDownButton.Parent := self;
    FTrackPanel.Parent := self;
    FThumbShape.Parent := FTrackPanel;

    // tell the IDE to store the modified properties
    FUpButton.setSubComponent(true);
    FDownButton.setSubComponent(true);
    FTrackPanel.setSubComponent(true);
    FThumbShape.setSubComponent(true);

    // ensure sub controls cannot be selected from the IDE
    FUpButton.ControlStyle:= FUpButton.ControlStyle - [csNoDesignSelectable];
    FDownButton.ControlStyle:= FDownButton.ControlStyle - [csNoDesignSelectable];
    FTrackPanel.ControlStyle:= FTrackPanel.ControlStyle - [csNoDesignSelectable];
    FThumbShape.ControlStyle:= FThumbShape.ControlStyle - [csNoDesignSelectable];

    FUpButton.Name := 'UpButton';
    FDownButton.Name := 'DownButton';
    FTrackPanel.Name:='TrackPanel';
    FThumbShape.Name:='ThumbShape';

    // link events

    // used to scroll & start timer repeat
    FUpButton.OnMouseUp := @OnMouseUpDown;
    FUpButton.OnMouseDown:=@OnMouseUpDown;
    FDownButton.OnMouseUp:=@OnMouseUpDown;
    FDownButton.OnMouseDown:=@OnMouseUpDown;
    FTrackPanel.OnMouseUp:=@OnMouseUpDown;
    FTrackPanel.OnMouseDown:=@OnMouseUpDown;

    FTimer.OnTimer:=@OnTimer;

    // used to change theme
    FUpButton.OnMouseEnter:=@WhenMouseEnter;
	FDownButton.OnMouseEnter:=@WhenMouseEnter;
    FTrackPanel.OnMouseEnter := @WhenMouseEnter;
    FThumbShape.OnMouseEnter := @WhenMouseEnter;

    // used to change theme and disable timer
    FUpButton.OnMouseLeave := @WhenMouseLeave;
    FDownButton.OnMouseLeave := @WhenMouseLeave;
	FTrackPanel.OnMouseLeave := @WhenMouseLeave;
    FTrackPanel.OnExit:= @WhenMouseLeave;
    FThumbShape.OnMouseLeave := @WhenMouseLeave;

    // used to move thumb
    FThumbShape.OnMouseDown:=@OnThumbMouseUpDown;
    FThumbShape.OnMouseMove := @OnThumbMouseMove;

    // apply default appearence
    Init();

end;

destructor TBasicScrollbar.Destroy;
begin
    FTheme.Free;
    _CurrColorSet.Free;
	inherited Destroy;
end;

procedure TBasicScrollbar.Init;
begin
    self.BevelOuter:=bvNone;
    self.BevelInner:=bvNone;
    self.Caption := '';
    // set width to OS scrollbar default width
    self.Width := GetSystemMetrics(SM_CXVSCROLL);

    FTrackPanel.Align:=alClient;
    FTrackPanel.Caption := '';
    FTrackPanel.BevelOuter:=bvNone;
    FTrackPanel.BevelInner:=bvNone;

    FUpButton.Align:=alTop;
    FUpButton.Caption := '▲';
    FUpButton.Flat := true;
    FUpButton.Transparent:=false;

    FDownButton.Align:=alBottom;
    FDownButton.Caption := '▼';
    FDownButton.Flat := true;
    FDownButton.Transparent:=false;

    FThumbShape.Visible := false;
    FThumbShape.width := FTrackPanel.Width;
    FThumbShape.Pen.Style := psClear;
    FThumbShape.Pen.Width := 0;

    SetThemeRec(BASICSCROLLBAR_LIGHT_THEME);
end;

procedure TBasicScrollbar.WhenMouseEnter(Sender:TObject);
begin
 ApplyTheme();
end;

procedure TBasicScrollbar.WhenMouseLeave(Sender: TObject);
begin
  ApplyTheme();
  FTimer.Enabled:=false;
end;

procedure TBasicScrollbar.OnMouseUpDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var _event: TBasicScrollbarEventType;
begin
	FTimer.Enabled:=false;

    // default to none
    _event := BS_NONE;

    // if mouse is down, shall we start the timer ?
    if ssLeft in Shift then begin
      //debugln('mouse down');
      if Sender = TrackPanel then begin
        if Y < FThumbShape.top then
        	_event := BS_PAGE_UP
        else if Y > FThumbShape.Top + FThumbShape.height then
        	_event := BS_PAGE_DOWN
	  end
      else if Sender = UpButton then
          _event := BS_LINE_UP
      else if Sender = DownButton then
          _event := BS_LINE_DOWN;
	end;
    //else debugln('mouse up');

    if _event <> BS_NONE then begin
      // trigger the event
      ScrollEvent(_event);
      // start the timer
      FTimer.Tag := Int64(_event);
      FTimer.Interval:=350;
      FTimer.Enabled:=true;
	end;
end;

procedure TBasicScrollbar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
begin
if (ThumbShape.Tag > 0) and (ssLeft in Shift) then begin

  // move the thumb in the track
    ThumbShape.Top := ThumbShape.Top + Y - ThumbShape.Tag;
    if (ThumbShape.Top < 0) then
    	ThumbShape.Top := 0
    else if (ThumbShape.Top + ThumbShape.Height > TrackPanel.Height) then
    	ThumbShape.Top := TrackPanel.Height - ThumbShape.Height;

    // trigger a scroll event
    ScrollEvent(BS_THUMB_MOVE);
    end;
end;

procedure TBasicScrollbar.OnThumbMouseUpDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//debugln('thumbUpDown');
    FTimer.Enabled:=false;
    if ssLeft in Shift then
        FThumbShape.Tag := Y // mouse initial position
    else
      FThumbShape.Tag := 0;
end;

procedure TBasicScrollbar.OnTimer(Sender: TObject);
begin
	FTimer.Enabled:=false;

    if FTimer.Tag > 0 then begin
      ScrollEvent(TBasicScrollbarEventType(FTimer.Tag));
      FTimer.Interval:=70;
      FTimer.Enabled:=true;
	end;
end;

procedure TBasicScrollbar.ScrollEvent(Event: TBasicScrollbarEventType);
begin
if FOnScrollEvent <> nil then
    FOnScrollEvent(self, Event);
end;

procedure TBasicScrollbar.SetEnabled(val: boolean);
begin
	if val <> FEnabled then begin
		FEnabled := val;
        FThumbShape.Visible:=FEnabled;
		FUpButton.Enabled:=FEnabled;
		FDownButton.Enabled:=FEnabled;
        ApplyTheme();
        Changed;
	end;
    inherited SetEnabled(val);
end;

procedure TBasicScrollbar.SetTheme(ATheme: TBasicScrollbarTheme);
begin
    FTheme.Assign(ATheme);
    ApplyTheme();
    // buttons background is not status dependent - may code this if needed
    FUpButton.Color := ATheme.Inactive.Track;
    FDownButton.Color := ATheme.Inactive.Track;

    Changed;
end;

procedure TBasicScrollbar.SetThemeRec(AThemeRec: TBasicScrollbarThemeRec);
begin
	FTheme.loadFromRec(AThemeRec);
    ApplyTheme();
    // buttons background is not status dependent - may code this if needed
    FUpButton.Color := AThemeRec.Inactive.Track;
    FDownButton.Color := AThemeRec.Inactive.Track;

    Changed;
end;

procedure TBasicScrollbar.SetThinnerInactiveThumb(Value: boolean);
begin
    if Value <> FThinnerInactiveThumb then begin
        FThinnerInactiveThumb := Value;
        ApplyTheme();
        Changed;
	end;
end;

procedure TBasicScrollbar.OnThemeChanged(Sender: TObject);
begin
    ApplyTheme();
end;

procedure TBasicScrollbar.ApplyTheme;
var _newW:integer;
begin
    if FEnabled then begin
        if self.MouseInClient then begin
            ApplyColorSet(FTheme.Active);
            // compare before, because setting left & width may be heavy
            if FThumbShape.left <> 0 then begin
	            FThumbShape.Left := 0;
    	        FThumbShape.Width:=FTrackPanel.Width;
			end;
		end
        else begin
           ApplyColorSet(FTheme.Inactive);
           if FThinnerInactiveThumb then begin
               _newW := round(FTrackPanel.Width/4);
               // compare before, because setting left & width may be heavy
               if FThumbShape.Width > _newW then begin
	               FThumbShape.Width:=_newW;
    	           FThumbShape.Left := ceil((FTrackPanel.Width - _newW)/2);
			   end;
		   end;
		end;
	end
    else
    	ApplyColorSet(FTheme.NoScroll);
end;

procedure TBasicScrollbar.ApplyColorSet(AColorSet: TBasicScrollbarColorSet);
begin
	if AColorSet.Thumb <> _currColorSet.Thumb then begin
		FThumbShape.Brush.Color := AColorSet.Thumb;
		_currColorSet.Thumb:= AColorSet.Thumb;
	end;
	if AColorSet.Track <> _currColorSet.Track then begin
		FTrackPanel.Color:=AColorSet.Track;
		_currColorSet.Track := AColorSet.Track;
	end;
	if AColorSet.Buttons <> _currColorSet.Buttons then begin
		FUpButton.Font.Color := AColorSet.Buttons;
		FDownButton.Font.Color := AColorSet.Buttons;
		_currColorSet.Buttons := AColorSet.Buttons;
	end;
end;

initialization
{$I BasicScrollbar.lrs}
end.

