unit FSVclBase;

interface

uses
  SysUtils, Classes, Windows, Controls;

type
  TMouseFlags = (mfMouseOver, mfLButtonDown, mfRButtonDown);
  TMouseFlag = set of TMouseFlags;

  TNotifyID = (niChange, niDestroy);
  TExNotifyEvent = procedure(Sender: TObject; ID: TNotifyID) of object;

procedure GetMsgCursor(out pt: TPoint);

implementation

procedure GetMsgCursor(out pt: TPoint);
var
  combine: DWORD;
begin
  combine := GetMessagePos;
  pt.X := combine and $ffff;
  pt.Y := combine shr 16;
end;

end.
