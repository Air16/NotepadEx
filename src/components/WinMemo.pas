(*
						WinMemo - Copyright Air16 2022

	This unit contains a TWinMemo and a custom scrollbar to replace TMemo
    native scrollbar.

    Written as an exercise to code a Notepad clone with unlimited undo/redo,
    auto-backup and a dark theme, with the same performance as Notepad original.

    Important : a TWinMemoScrollbar linked to TWinMemo using Memo property works only
    if TWinMemo vertical scrollbar is enabled. I didn't find any way to enable
    TMemo vertical scrollbar while hiding it.
    The workaround is the following (see NotepadEx application for a working example) :
    1. TWinMemo.Scrollbars = ssAutoBoth (or ssVertical or ssAutoVertical)
    2. TWinMemo.align := alLeft (not alClient) + all 4 anchors set
    3. TwinMemoScrollbar.align := alRight
    4. in FormCreate : TWinMemo.width := Form1.width, so that TWinMemo behaves
       as if its align property was set to 'alClient'
	5. call SetWindowPos() when needed, to keep the scrollbar on top of TWinMemo

*)
unit WinMemo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, BasicScrollbar, CommCtrl, Windows;

type

  { TWinMemo }

  (* TWinMemo is a TMemo using Windows API to fill the gap :
  - OnScroll event, so that it may be linked with a custom scrollbar
  - Scroll methods (page up/down, line up/down...)

  The tricky part is to detect OnScroll.
  For this we use a dedicated WndProc for Parent so that we can receive EN_VSCROLL
  messages via WM_COMMAND.
  EN_VSCROLL notifies all OnScroll events except :
  - when the control is resized => we override DoOnResize()
  - when a new line is added inside the Memo => we override Change()

  Performance is very good and TWinMemo can replace TMemo whatever the needs.
  When the OnScroll detection is not needed, just leave OnScroll empty and
  the TWinMemo behaves exactly as a TMemo.
  *)

  TWinMemo = class(TMemo)
  private
    _lastLineCount:integer;
    FOwnerHandle:THandle;
  protected
    FOnScroll:TNotifyEvent;
    procedure NotifyScroll();
    procedure DoOnResize(); override;
    procedure Change(); override;
    procedure SetOnScroll(EventProc:TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure SetMargins(LeftMargin:integer;RightMargin:integer);
    (* LineCount, VisibleLineCount and Position call GetScrollInfo internally
       without caching, so it is better to keep functions here vs properties *)
    function GetLineCount:integer; // total lines
    function GetVisibleLineCount:integer; // visible lines = 1 page
    function GetVScrollbarPosition:integer; // 1 < position < lineCount - VisibleLineCount
    (* straight Windows API calls *)
    procedure VScrollPageUp(); // one page up
    procedure VScrollPageDown(); // one page down
    procedure VScrollLineUp(Value: integer = 1); // one line up
    procedure VScrollLineDown(Value: integer = 1); // one line down
    procedure VScrollToPosition(position:integer); // 1 < position < lineCount - visibleLineCount

  published
	property OnScroll: TNotifyEvent read FOnScroll write SetOnScroll;

  end;

  { TBasicScrollbar }

  { TWinMemoScrollbar }

  TWinMemoScrollbar = class(TBasicScrollbar)
    private
      FMemo:TWinMemo;
      FMemoScroll:TNotifyEvent;
      FWheelScrollStep:integer;
    protected
      procedure UpdateThumb();
      procedure SetMemo(AMemo:TWinMemo);
      procedure OnMemoScrolled(Sender:TObject);
      procedure ScrollEvent(Event:TBasicScrollbarEventType); override;
      // implement Wheel over the scrollbar - not available in BasicScrollbar because
      // the code is OS sensitive
	  function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
	  function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure SetWheelScrollStep();
    public
      constructor Create(AOwner:TComponent); override;
    published
      property Memo:TWinMemo read FMemo write SetMemo;
      property WheelScrollStep:integer read FWheelScrollStep;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Air16 Windows',[TWinMemo, TWinMemoScrollbar]);
end;

const
  CSubClassId = 132;

function _WinMemoParentWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM; uIdSubclass: UINT_PTR; memHandle: DWORD_PTR): LRESULT; stdcall;
// used to get wParamhi from wParam
var _msg:TMessage;
begin
  // default WinProc
  Result := DefSubclassProc(Wnd, Msg, wParam, lParam);
  // if it is a WM_COMMAND with EN_VSCROLL message for us, call its target Memo
  if (uIdSubclass = CSubClassId) and (Msg = WM_COMMAND) then begin // WM_COMMAND
    _msg.wParam:=wParam;
    if (_msg.wParamhi = EN_VSCROLL) then
	    TWinMemo(memHandle).NotifyScroll();
  end;
end;

constructor TWinMemo.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
    // store owner handle for SetOnScroll & Destroy
    FOwnerHandle := TWinControl(AOwner).Handle;
end;

procedure TWinMemo.Change;
var lc:integer;
begin
  if FOnScroll <> nil then begin
    // update lastLineCount
    lc := VertScrollbar.Range;
    if lc <> _lastLineCount then begin
      _lastLineCount := lc;
      // and trigger a Scroll Event if it has changed
      NotifyScroll();
	end;
  end;
  inherited Change;
end;

destructor TWinMemo.Destroy;
begin
    if FOnScroll <> nil then
		RemoveWindowSubclass(FOwnerHandle, @_WinMemoParentWndProc, CSubClassId);
	inherited Destroy;
end;

procedure TWinMemo.DoOnResize;
begin
	inherited DoOnResize;
    NotifyScroll();
end;

procedure TWinMemo.NotifyScroll;
begin
    if FOnScroll <> nil then
        FOnScroll(self);
end;

function TWinMemo.GetLineCount: integer;
begin
    // updated in Change() => always up to date
    Result := _lastLineCount;
end;

function TWinMemo.GetVisibleLineCount: integer;
begin
	Result := VertScrollbar.Page;
end;

function TWinMemo.GetVScrollbarPosition: integer;
begin
    Result := VertScrollbar.Position;
end;

procedure TWinMemo.SetMargins(LeftMargin:integer;RightMargin:integer);
var msg:TMessage;
begin
    msg.lParamlo:=LeftMargin;
    msg.lParamhi:=RightMargin;
	SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, msg.lParam);
end;

procedure TWinMemo.SetOnScroll(eventProc: TNotifyEvent);
begin
if FOnScroll <> nil then begin
	RemoveWindowSubclass(FOwnerHandle, @_WinMemoParentWndProc, CSubClassId);
	FOnScroll := nil;
end;

if EventProc <> nil then begin
    // register another WinProc for the owner so that we can receive EN_VSCROLL event
	if not SetWindowSubclass(FOwnerHandle, @_WinMemoParentWndProc, CSubClassId, DWORD_PTR(self)) then
  		RaiseLastOSError;
    // refresh lastLineCount
    _lastLineCount := VertScrollbar.Range;
    // assign
	FOnScroll := eventProc;
end;
end;

procedure TWinMemo.VScrollLineDown(Value: integer);
var i:integer;
begin
	for i:=1 to Value do
	    SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TWinMemo.VScrollLineUp(Value: integer);
var i:integer;
begin
	for i:=1 to Value do
	    SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
end;

procedure TWinMemo.VScrollPageDown;
begin
  	SendMessage(Handle, WM_VSCROLL, SB_PAGEDOWN, 0);
end;

procedure TWinMemo.VScrollPageUp;
begin
  	SendMessage(Handle, WM_VSCROLL, SB_PAGEUP, 0);
end;

procedure TWinMemo.VScrollToPosition(position: integer);
var msg:TMessage;
begin
    msg.wParamlo:=SB_THUMBTRACK;
    msg.wParamhi:=position;
    SendMessage(Handle, WM_VSCROLL, msg.wParam, 0);

end;

{ TWinMemoScrollbar }

constructor TWinMemoScrollbar.Create(AOwner: TComponent);
begin
	SetWheelScrollStep();
	inherited Create(AOwner);
end;

function TWinMemoScrollbar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
	if FWheelScrollStep <= 0 then
	    ScrollEvent(BS_PAGE_DOWN)
	else
		ScrollEvent(BS_WHEEL_DOWN);

	Result:=inherited DoMouseWheelDown(Shift, MousePos);
end;

function TWinMemoScrollbar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
	): Boolean;
begin
    // this code is Windows specific
	if FWheelScrollStep <= 0 then
        ScrollEvent(BS_PAGE_UP)
	else
    	ScrollEvent(BS_WHEEL_UP);

	Result:=inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TWinMemoScrollbar.SetWheelScrollStep();
var rawVal:uint32; // we need an unsigned
begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @rawVal, 0);
    if rawVal = WHEEL_PAGESCROLL then
	    FWheelScrollStep:=0
    else
        FWheelScrollStep := rawVal;
end;

procedure TWinMemoScrollbar.OnMemoScrolled(Sender: TObject);
begin
  //debugln('   onMemoScrolled');
  UpdateThumb();
    if FMemoScroll <> nil then
    	FMemoScroll(FMemo);
end;

procedure TWinMemoScrollbar.ScrollEvent(Event: TBasicScrollbarEventType);
var targetThumbPosition: Integer;
begin
  //debugln('   ScrollEvent');
  if FMemo <> nil then begin
    Case Event of
		BS_PAGE_UP: FMemo.VScrollPageUp();
		BS_PAGE_DOWN: FMemo.VScrollPageDown();
		BS_LINE_UP: FMemo.VScrollLineUp();
		BS_LINE_DOWN: FMemo.VScrollLineDown();
        BS_WHEEL_UP: FMemo.VScrollLineUp(FWheelScrollStep);
        BS_WHEEL_DOWN: FMemo.VScrollLineDown(FWheelScrollStep);
        BS_THUMB_MOVE: begin
            // target memo thumb position
  			targetThumbPosition := round((FMemo.GetLineCount - FMemo.GetVisibleLineCount + 1) * ThumbShape.Top / (TrackPanel.Height - ThumbShape.Height));
        	// build windows message
            FMemo.VScrollToPosition(targetThumbPosition);
		end;
	end;
    //UpdateThumb();
  end;

  inherited ScrollEvent(Event);
end;

procedure TWinMemoScrollbar.SetMemo(AMemo: TWinMemo);
begin
	// restore old OnScroll event
	if FMemo <> nil then
		FMemo.OnScroll := FMemoScroll;

    // set new val
	FMemo := AMemo;

	// intercept its OnScroll event
    if AMemo <> nil then begin
	    FMemoScroll := AMemo.OnScroll;
    	AMemo.OnScroll:=@OnMemoScrolled;
	end;

    // update thumb
    UpdateThumb();
end;

procedure TWinMemoScrollbar.UpdateThumb;
var bTop, bHeight:double;
  lc, vlc:integer;
begin
	bTop := -1;
	bHeight := 0;

	if FMemo = nil then begin
      Enabled := false;
	  exit;
	end;

	// Page = visible height
	// Range = logical height (= total height)
	// Position in [1..Range-Page] when visible, 0 otherwise

	// calc bTop and bHeight as percentage
	with FMemo do begin
		lc := GetLineCount;
		vlc := GetVisibleLineCount;
		if lc >= vlc then begin
			bTop := VertScrollbar.Position*100/(lc+1);
			bHeight := vlc*100/(lc+1);
		end;
	end;

	//debugln('      lc='+s(lc), ' vlc='+s(vlc), ' bTop='+s(bTop)+'%', ' bHeight='+s(bHeight)+'%');

	if bTop < 0 then begin
		Enabled := false;
		exit;
	end;

	Enabled := true;

	// ok, draw the thumb
	FThumbShape.Height:= round(FTrackPanel.Height * bHeight / 100);
	FThumbShape.Top:=round(FTrackPanel.Height * bTop / 100);
end;

initialization
{$I WinMemo.lrs}

end.
