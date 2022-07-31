
**NotepadEx** is born as an exercice to code the famous Windows Notepad application using Lazarus, with equivalent performance, and two added features :
- Unlimited undo/redo
- Automatic backup and restore of unsaved files

Later, a dark theme were added with a fast `CTRL+M` toggle switch.

Encoding & CRLF management, zoom, open/save dialogs with the 'encoding' combo, command line parameters, keyboard shortcuts, status bar... Each of Notepad features is mirrored in NotepadEx. Win32 Edit control is used to keep features, performance and look and feel as close to the Windows Notepad application as possible.

NotepadEx is a ~3MB single .exe file without setup, that can be used as a drop-in replacement of Notepad.

The added features are grouped into a new `Ex` Menu.

## Auto-backup
Simple & easy : 
- when the content has changed, it is saved into the Backup folder
- when the content is saved, the corresponding backup file is deleted
- if the application is killed, the remaining backup files are restored the next time the application is loaded.

The backup folder is set to `%APP_DATA%/NotepadEx/Backup`. It is possible to open it quickly using Ex menu.

Changes are saved when the application is idle for at least 2 seconds.

The second panel of the status bar shows the current status of Auto-backup :
- `disabled`
- `OK` : when the content has been saved
- `...` : when a change is detected and not saved yet. 

The content size (in Bytes) is also displayed in this panel.

## Unlimited undo/redo
Simple and easy too : 
- a new version is pushed on top of the stack when a new word is typed or when the content has changed (paste, cut...).
- on `CTRL+Z`, the previous version is restored
- on `CTRL+Y`, the next version is restored, if any

The fist panel of the status bar shows the current index and the total stack size.

> Each stack item contains a full content version. No diff nor any intelligence here. On rare edge cases (like a very large file and a big history stack size), this may lead to huge RAM consumption. But well... Nowadays computers with gigabytes of RAM can handle this without any problem, and it's Notepad, not Word nor Visual Studio.

## Dark theme
Press `CTRL+M` to switch back and forth.

The Dark Theme uses the exact same win32 Edit control with a black background and a light grey text color ; performance is identical.

Added to this :
- status bar is hidden
- menu is hidden
- a dark scrollbar is displayed, with a thinner thumb

> As Win32 scrollbar offers no way to customize colors, a full scrollbar component were coded from scratch. See TBasicScrollbar component.

## Additional features and notes

### Status bar
When `Show Caret position instead of LnCol` is checked in `Ex` menu, the caret position is shown as a number in [0 .. text length].


### Line ending chars
Even when a file is marked as CR or LF, the editor uses Windows default ending
chars CR+CF. Thus a new line adds 2 chars. When the file is saved, the conversion
to the marked line ending occurs.

3 or 4 bytes characters may be counted as 2 characters. This is how Windows internal Edit control works.

### Settings storage
Settings are stored in Windows registry : `HKCU/Software/NotepadEx`

### install/uninstall
NotepadEx is a standalone .exe file. 

On first load, it creates :
- the Settings storage key in Windows registry (see above)
- the Backup folder (see above)

To make it the default .txt program :
- create a `NotepadEx` folder into `C:/Program Files/` and copy `NotepadEx.exe` into it
- on any .txt file : right click, `Open with`, then choose NotepadEx.exe and check `Always use this program`.

To remove it completely from Windows, delete the Settings storage key and the Backup folder.


# History

### v1.2.0
- UI : Improved status bar display
- Dark theme : Added scrollbar mouse wheel support

- Code : moved all generic code into a package : TBasicScrollbar, TWinMemo, TWinMemoScrollbar,
TWinOpenDialog, TWinSaveDialog, TEasyRegistry + LogUtils, LineEndingUtils & Air16Utils

### v1.1.5
- Dark theme : Added scrollbar up/down buttons

### v1.1.4
- Fixed an issue when changing Windows Display Scale

### v1.1.3
- Dark theme : Updated dark theme backup file background color
- Dark theme : Reduced inactive scrollbar width
- Dark theme : Fixed form/memo margin issue with dark theme

### v1.1.2
- UI : Set focus to Memo on start

### v1.1.1
- Dark theme : Added thumb scrolling + page up/page down

### v1.1.0
- Added dark theme

### v1.0
- Initial version with auto-backup and unlimited undo/redo


# Source code
The full source code is available under MIT or LGPL license.

As i spent more time than expected gathering information about Lazarus and Win32, i've decided to highlight every specific feature into a dedicated component or unit, so that it can be understood and reused easily.

`TWinMemo` is a TMemo children able to detect scroll events and set left & right margins.

`TWinOpenDialog` and `TWinSaveDialog` are drop-in replacements of TOpenDialog and TSaveDialog, with the ability to insert an extra control (Combo or Checkbox).

`TBasicScrollbar` is a multiplatform vertical scrollbar component coded from scratch using panels, speedbuttons and a shape. Almost everything can be customized, and there is a `Theme` property and utilities to easily load and change themes.

`TWinMemoScrollbar` is a TBasicScrollbar children with a `Memo` property, so that it can be synced with a `TWinMemo` and replace the `TWinMemo` default vertical scrollbar.

`TEasyRegistry` is a very light component for one-line basic common read/write usage of TRegistry.

`LineEndingUtils` contains CRLF detection code.

# Folders
`src/project` contains the Lazarus project

`src/components` contains components & units (extracted from a large package) with their .lrs icon files. Setup : create a new package in Lazarus, add the files and install the new package.


