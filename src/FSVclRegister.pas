unit FSVclRegister;

interface

uses
  SysUtils, Classes, Windows, DesignEditors, DesignIntf, DesignMenus, ComponentDesigner,
  FSTheme, FSGraphics, FSControls, FSStdCtrls, FSSkinCtrls, FSScrollControls, FSNavTree,
  FSEdit, FSTrayIcon;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FSVCL', [TFsTheme, TFsRectangle, TFsNinePitchDrawable, TFsPictureDrawable,
    TFsGradientDrawer, TFsImage, TFsImageButton, TFsCoverButton, TFsCheckBox, TFsCombobox,
    TFsSkinButton, TFsSkinCheckBox, TFsSkinCombobox,
    TFsPanel, TFsPageControl, TFsEdit, TFsButtonEdit, TFsFlatScrollBar,
    TFsMemo, TFsListBox, TFsListView, TFsTreeView, TFsNavTree, TFsTrayIcon]);
end;

end.
