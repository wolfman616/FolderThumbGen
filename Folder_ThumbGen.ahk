#NoEnv ; (MW:2023) (MW:2023)
#NoTrayicon
SetBatchLines,-1
#Persistent
#Singleinstance,Force
a_scriptStartTime:= a_tickCount 
Setworkingdir,% (splitpath(A_AhkPath)).dir
; #IfTimeout,200 ;* DANGER * : Performance impact if set too low. *think about using this*.
; ListLines,Off 
DetectHiddenWindows,On
DetectHiddenText,	On
SetTitleMatchMode,2
SetTitleMatchMode,Slow
SetWinDelay,-1
coordMode,	ToolTip,Screen
coordmode,	Mouse,	Screen
loop,parse,% "VarZ,Menus,Hookinit",`,
	 gosub,% a_loopfield
menu,tray,icon,% "HICON: " b64_2_hicon(smicon64)
menu,tray,icon
OnMessage(0x6,"activ8")
OnMessage(0x404,"AHK_NOTIFYICON")

;PNGThumb := TempDir . p.fn . ".png"

if(!isdir(Args1:=( !A_Args[1]? "C:\Users\ninj\Desktop11\New folder (4)" : A_Args[1] ))) {
	msgb0x("err " Args1 " not a folder.",4)
	exitapp,
}
TempDir:= A_Temp . "\"

AComspec:= comspec . " /c convert -thumbnail 256x256 " , ArgsForIM:= "-background none -virtual-pixel Background -distort Perspective "

global TargetDir:= Args1
, Thumb64TxtFile:= TempDir . "thumb64.txt"
, FolderFront:="FolderFront.png"
, FolderBack:="FolderBack.png"
, imageFiletypes:="png,jpg,gif,jfif,bmp"
, FileFront_alpha:=		62
, FileMiddle_alpha:=	82
, FileBack_alpha:=		87
FileName:=Args1 . "\desktop.ini"
; msgbox % text:=Base64_Encode_Text("C:\Users\ninj\Desktop11\New folder (4)")
; msgbox % text:=Base64_decode_Text(text)
; FolderName=%outdir%desktop.ini
foldernameb64:= Base64_Encode_Text(Args1)
, cacheRoot:="c:\out\cached\"
, cacheSubfolder:= Substr(foldernameb64,1,1)
, iconPath:= (CacheDir:=cacheRoot . cacheSubfolder) . "\" . foldernameb64 . ".ico"
ass:= GetThreeMostRecentFiles(Args1)
NewTop3:= ReorderStrImg1st(ass)

if(!fileexist(CacheDir))
	FileCreateDir,% CacheDir

loop,parse,% NewTop3,`/
	switch,a_index {
		case,1 : PNGThumb:=""
			splitpath,% a_loopfield,,,Ext
			if(!instr(Ext,"xcf")) {
				if(instr(imageFiletypes,Ext)) {
					PNGThumb:= a_loopfield
				} else { ;get the filetype icon detail from reg
					PNGThumb:= spunk(a_loopfield)
					commandstr:= acomspec chr(34) PNGThumb chr(34) "[0] " ArgsForIM chr(34) "0,0 44,4 0,256 44,196 256,0 148,29 256,256 147,254" chr(34) " -matte -channel A +level 0," FileFront_alpha "% +channel " top_:= chr(34) TempDir "front.png" chr(34)
					runwait,% commandstr,,hide
					continue,
				}
			} else {
				p:= splitpath(xcf:= a_loopfield)
				gosub,GimpThumbGenerate
				sleep,1000
			} commandstr:= acomspec chr(34) PNGThumb chr(34) " " ArgsForIM chr(34) "0,0 44,4 0,256 44,196 256,0 148,29 256,256 147,254" chr(34) " -matte -channel A +level 0," FileFront_alpha "% +channel " top_:= chr(34) TempDir "front.png" chr(34)
			runwait,% commandstr,,hide
		case,2: PNGThumb:=""
			splitpath,% a_loopfield,,,Ext
			if(!instr(Ext,"xcf")){
				if(instr(imageFiletypes,Ext)) {
					PNGThumb:= a_loopfield
				} else { ;get the filetype icon detail from reg
					PNGThumb:= spunk(a_loopfield)
					commandstr:= acomspec chr(34) PNGThumb chr(34) "[0] " ArgsForIM chr(34) "0,0 44,6 0,256 44,198 256,0 185,32 256,256 185,252" chr(34) " -matte -channel A +level 0," FileMiddle_alpha "% +channel " mid_:= chr(34) TempDir "middle.png" chr(34) 
					runwait,% commandstr,,hide
					continue,
				}
			} else {
				p:= splitpath(xcf:= a_loopfield)
				gosub,GimpThumbGenerate
			} commandstr:= acomspec chr(34) PNGThumb chr(34) " " ArgsForIM chr(34) "0,0 44,6 0,256 44,198 256,0 185,32 256,256 185,252" chr(34) " -matte -channel A +level 0," FileMiddle_alpha "% +channel " mid_:= chr(34) TempDir "middle.png" chr(34) 
				runwait,% commandstr,,hide
		case,3: PNGThumb:=""
			splitpath,% a_loopfield,,,Ext
			if(!instr(Ext,"xcf")) {
				if(instr(imageFiletypes,Ext)) { 
					PNGThumb:=a_loopfield
				} else { ;get the filetype icon detail from reg
					PNGThumb:= spunk(a_loopfield)
					commandstr:=acomspec chr(34) PNGThumb chr(34) "[0] " ArgsForIM chr(34) "0,0 44,3 0,256 44,197 256,0 219,33 256,256 218,250" chr(34) " -matte -channel A +level 0," FileBack_alpha "% +channel " bot_:= chr(34) TempDir "back.png" chr(34)
					runwait,% commandstr,,hide
					continue,
				}
			} else {
				p:= splitpath(xcf:= a_loopfield)
				gosub,GimpThumbGenerate
			} commandstr:=acomspec chr(34) PNGThumb chr(34) " " ArgsForIM chr(34) "0,0 44,3 0,256 44,197 256,0 219,33 256,256 218,250" chr(34) " -matte -channel A +level 0," FileBack_alpha "% +channel " bot_:= chr(34) TempDir "back.png" chr(34)
			runwait,% commandstr,,hide
	}

if(!FileLayersConfirm())
	msgbox,error creating file layers
folder_thumb_combine:
cmd_string:= acomspec "-background none -layers merge " FolderBack " " bot_ " " mid_ " " top_ " " FolderFront " -matte -channel A +level 0,95% +channel " Resultfile:= chr(34) TempDir "result.png" chr(34)
runwait,% cmd_string,% a_scriptdir,hide

 cmd_string2:= ComSpec " /C convert " Resultfile " -define icon:auto-resize=" resize " " iconPath,,hide

run,% cmd_string2,% a_scriptdir,hide

gosub,DesktopIni

return,


;PNG_eXist:
;loop {
;	if FileExist(out3) {
		RunWait, %comspec% /c convert %out3% -define icon:auto-resize=%resize% %NIGSSSS%,,hide
	;	break
;	} else {
	;	sleep,120
;		goto,PNG_eXist
;}	}

loop 5 {
	if FileExist(Out4) {
		FileMove,Out4,Dest_Dir
		break,
	}
	else,sleep,80
}

Dest_File=%Dest_Dir%\%inNameNoExt%.ico

loop,10 {
	sleep,400
	if(FileExist(JFIFThumb))
		break,
	else,msgbox no %JFIFThumb%
}

FileGetSize,nBytes,%JFIFThumb%
FileRead,Bin,*c %JFIFThumb%

if(!thumb64:= Base64Enc(Bin,nBytes))
	msgbox,err no b64 output from result jfif

loop,10 {
	if(!FileExist(Thumb64TxtFile)) {
		FileApPend,% thumb64,% Thumb64TxtFile
		break,
	} else {
		filedelete,% Thumb64TxtFile
		sleep,500
	}	sleep,100
}

loop,10 {
	sleep,500
	if(FileExist(Thumb64TxtFile))
		break,
} ;loop,parse,% "Resultfile,JFIFThumb,top_,mid_,bot_",`,

loop,parse,% "Resultfile,top_,mid_,bot_",`,
	filedel(%a_loopfield%)
return,

spunk(path) {
	regRead,default,% Regki:="HKEY_CLASSES_ROOT\." . xt:= (p:= splitpath(path)).ext
	if(!default) {
		regRead,defaulticon,% Regki:="HKEY_CLASSES_ROOT\." . xt . "\DefaultIcon"
		if(!defaulticon)
			msgbox,% "no default set "  xt " " path
		else {
			(instr(defaulticon,"%SystemRoot%")? defaulticon:= strreplace(defaulticon,"%SystemRoot%","C:\Windows"))
			(instr(defaulticon,"%1")||instr(defaulticon,"%l")?  defaulticon:= "C:\Icon\FileTypes\davinci.ico")
		}
	} else {
		regRead,ext_desc,% Regki:= "HKEY_CLASSES_ROOT\" . default
		regRead,defaulticon,% Regki . "\DefaultIcon"
		if !defaulticon
			msgbox,% "no default set "  xt " " path
		else {
			xtt:= chr(0x27) . "." . xt . chr(0x27)
			(instr(defaulticon,"%SystemRoot%")? defaulticon:= strreplace(defaulticon,"%SystemRoot%","C:\Windows"))
			(instr(defaulticon,"%1")||instr(defaulticon,"%l")?  defaulticon:= "C:\Icon\FileTypes\davinci.ico")
			defi:= chr(0x27) . defaulticon . chr(0x27)
		}
	} w:= regexmatch(defaulticon,"(\,\-?\d+)")? Msgbicon:= regexreplace(defaulticon,"(\,\-?\d+)","") :  Msgbicon:= defaulticon
	return,Msgbicon
}

GimpThumbGenerate: ; GimpThumbGenerate(xcffile,position="")
;msgbox % xcf " xcf"
;msgbox % 	PNGThumb := TempDir . p.fn . ".png"
PNGThumb := TempDir . "result.png"
 PNGThumb2:= TempDir . p.fn . "result-nq8.png"  ; Pallete quantized and limited to 256.
; JFIFThumb:= TempDir . p.fn . "result-nq8.JFIF" ; this was the only method to manifest a working alpha channel that i found .
;, Thumb64TxtFile:= p.dir . "\" . p.fn . ".txt"
;, Thumb64TxtFile:= "C:\Users\ninj\AppData\Local\Temp\thumb64.txt"
, cmdStr1:= (ComSpec " /C convert -background none -layers merge " chr(34) xcf chr(34) " " chr(34) PNGThumb chr(34)) ;produced final composite of layers
, cmdStr102:= (ComSpec " /C convert -thumbnail 256x256 " chr(34) PNGThumb chr(34) " " chr(34) PNGThumb chr(34)) ;produced final composite of layers
, cmdStr105:= "C:\Apps\pngnq-s9-2.0.2\pngnq-s9.exe -f -s1-A " chr(34) PNGThumb chr(34)
;, cmdStr2:= ComSpec " /C convert " chr(34) PNGThumb2 chr(34) " " chr(34)  JFIFThumb chr(34) 

;loop,parse,% "Thumb64TxtFile,PNGThumb,JFIFThumb",`,
;	if(FileExist(%a_Loopfield%))
;		FileDelete,% %a_Loopfield%

if(!FileExist(PNGThumb))
	run,% cmdStr1,% p.dir,hide
;msgbox 1 complete check result.png
loop,10 {
	sleep,400
	if(FileExist(PNGThumb))
		break,
} return,

loop,10 {
	sleep,400
	if(FileExist(PNGThumb))
		break,
}

run,% cmdStr102,% p.dir,hide
sleep,400

run,% cmdStr105,% p.dir,hide

loop,10 {
	sleep,500
	if(FileExist(PNGThumb2))
		break,
}
pngthumb:=PNGThumb2
;runwait,% cmd_string2,% a_scriptdir,hide
return,
;this where we deviate to make out angled version

;switch,position {
;	case,"front":	
;		string:= acomspec chr(34) PNGThumb chr(34) " " ArgsForIM chr(34) "0,0 44,4 0,256 44,196 256,0 148,29 256,256 147,254" chr(34) " -matte -channel A +level 0," FileFront_alpha "% +channel " top_:= chr(34) TempDir "front.png" chr(34)
;						runwait,% string
;
;	case,"middle":
;		runwait,% acomspec chr(34) PNGThumb chr(34) " " ArgsForIM chr(34) "0,0 44,6 0,256 44,198 256,0 185,32 256,256 185,252" chr(34) " -matte -channel A +level 0," FileMiddle_alpha "% +channel " mid_:= chr(34) TempDir "middle.png" chr(34) 
;
;	case,"back":
;		runwait,% acomspec chr(34) a_loopfield chr(34) " " ArgsForIM chr(34) "0,0 44,3 0,256 44,197 256,0 219,33 256,256 218,250" chr(34) " -matte -channel A +level 0," FileBack_alpha "% +channel " bot_:= chr(34) TempDir "back.png" chr(34),,hide
;		
;	default: gosub,createNawait_regthumb
;}

createNawait_regthumb:
run,% cmdStr102,% p.dir,hide
sleep,400

run,% cmdStr105,% p.dir,hide

loop,10 {
	sleep,500
	if(FileExist(PNGThumb2))
		break,
} if(FileExist(PNGThumb))
	FileDelete,% PNGThumb

run,% cmdStr2,% p.dir,hide

loop,10 {
	sleep,800
	if(FileExist(JFIFThumb))
		break,
}

loop,10 {
	sleep,800
	if(!FileExist(PNGThumb2))
		break,
	else,FileDelete,% PNGThumb2
}

FileGetSize,nBytes,%JFIFThumb%
FileRead,Bin,*c %JFIFThumb%
thumb64:= Base64Enc(Bin,nBytes)

loop,10 {
	if(!FileExist(Thumb64TxtFile)) {
		FileApPend,% thumb64,% Thumb64TxtFile
		break,
	} else,sleep,500
	sleep,100
}

loop,10 {
	sleep,500
	if(FileExist(Thumb64TxtFile))
		break,
}

loop,10 {
	sleep,80
	if(!FileExist(JFIFThumb))
		break,
		else,FileDelete,% JFIFThumb
}

FileDel(path2del="") {
	(instr(path2del,chr(34))? path2del:=strreplace(path2del,chr(34)))
	filedelete,%path2del%
	return,!errorlevel
}

; Function to get the three most recently modified files in the target directory
GetThreeMostRecentFiles(TargetDirectory) {
	FileList:= ""  ;Initialize an empty string to store the file list
	Loop,Files,%TargetDirectory%\*.*, , D	; Use FileRead, Loop to read the target directory and loop through each file
	{ FilePath:= A_LoopFileLongPath  ;Get the full path of the file
		FileGetTime,fileModifiedTime,%filePath%,M  ;Get the file modified time
		FileList .= filePath . "/" . fileModifiedTime . "`n" ; Append the file path and modified time to the FileList string
	}	Sort,FileList,R ; Sort the FileList based on modified time in descending order
	counter:=0
	Loop,30 { ; Get the three most recently modified files from the sorted list
		StringSplit,fileEntry,FileList,`n  ; Split the FileList into individual lines
		if(fileEntry1!="") {
			if(splitpath(fileEntry1).ext!="ini") {
				mostRecentFiles .= fileEntry1 . "/"
				FileList:= SubStr(FileList, StrLen(fileEntry1) + 2)  ; Remove the processed line from FileList
				counter++
			}
		} if(counter>2) {
			3done:= true
			break,
		}
	} return,(3done? Regexreplace(mostRecentFiles,"(20\d{12}\/)") :0)
}

ReorderStrImg1st(filesFSSV) { ; Split the comma-separated string into an array
	filesArray:= StrSplit(filesFSSV, ",")
		if(filesArray.Length()=3) { ; Check if there are exactly three file paths
			imageFiles:= [], nonImageFiles:= []
			Loop,% filesArray.Length() ; Determine the image files and non-image files
			{ ((FileHasImageExtension(filesArray[A_Index]))
				? imageFiles.Push(filesArray[A_Index]) 
				:  nonImageFiles.Push(filesArray[A_Index]))
			} ; Combine image files and non-image files in the desired order 
			for,i in nonImageFiles
				imageFiles.push(nonImageFiles[i]) 
			for,i in sortedFilesArray:= imageFiles ;totally repriritize image types which will possess thumbnails
				sortedFilesFSSV.=sortedFilesArray[i] . "/"
			return,SubStr(sortedFilesFSSV,1,-1) ; Remove the trailing slash and return the three most recently modified 
    } else,return,filesFSSV        ; If there are not exactly three file paths, return the original input
}

FileHasImageExtension(filePath) {
	static imageExtensions:= "xcf|jpg|jpeg|png|gif|bmp|tif|tiff|ico"
	fileType:= SubStr(filePath,InStr(filePath,".",0,- 1)+ 1)
	return,RegExMatch(imageExtensions, "\b" . fileType . "\b")
}

FileLayersConfirm() {
	local fail1,fail2,fail3
	loop,20 {
		loop,parse,% "back,middle,front",`,
			if(fileexist(TempDir . a_loopfield . ".png")){
				fail%a_index%:= False
				continue,
			}	else {
				sleep,40
				fail%a_index%:= True
			} if(!fail1&&!fail2&&!fail3)
				return,1
	}
}

DesktopIni:
msgbox % FileName "," iconPath

SetFolderIcon(FileName,iconPath)  
return,
if(FileExist(FileName)) {
	IniRead,OldIcon,%FileName%,.ShellClassInfo,IconResource
	if(Found_Previous_Pic2Icon:= Regexmatch(OldIcon,"i)(out)")) {
		OldIcon:= Regexreplace(OldIcon, "(,[0-9]{1,4})","")
		FileDelete,% OldIcon
	} FileDelete,% FileName
} Section1:=".ShellClassInfo", Section2 := "ViewState", Section3 := "ViewState", Section4 := "ViewState", Key1:= "IconResource", Key2 := "Mode", Key3 := "Vid", Key4 := "FolderType", Value2 := "", Value3 := "", Value4 := "Pictures"
Value1=%iconPath%,0
loop,4
	IniWrite,% Value%a_index%,%FileName%,% Section%a_index%,% Key%a_index%
;IniWrite,%Value1%,%FileName%,%Section1%,%Key1%
;IniWrite,%Value2%,%FileName%,%Section2%,%Key2%
;IniWrite,%Value3%,%FileName%,%Section3%,%Key3%
;IniWrite,%Value4%,%FileName%,%Section4%,%Key4%

Sleep,250
FileMoveDir,%FileName%,%FileName%,R
Sleep,50
FileSetAttrib,+SH,%FileName%,0,0
Sleep,50
FileSetAttrib,+S,%indir%,2,0
Sleep,100
FileMoveDir,%FileName%,%FileName%
Sleep,100
FileMoveDir,%FileName%,%FileName%,R
Sleep,500
run,%comspec% /c ie4uinit.exe -show,,hide
Sleep,50
SetFolderIcon(FileName,iconPath)  

exitapp,
return


SetFolderIcon(folderPath, iconPath, iconIndex:=0)  {
   static FCSM_ICONFILE := 0x10, FCS_FORCEWRITE := 0x2
   if !A_IsUnicode  {
      VarSetCapacity(WiconPath, StrPut(iconPath, "UTF-16")*2, 0)
      StrPut(iconPath, &WiconPath, "UTF-16")
   }
   VarSetCapacity(SHFOLDERCUSTOMSETTINGS, size := 4*5 + A_PtrSize*10, 0)
   NumPut(size, SHFOLDERCUSTOMSETTINGS)
   NumPut(FCSM_ICONFILE, SHFOLDERCUSTOMSETTINGS, 4)
   NumPut(A_IsUnicode ? &iconPath : &WiconPath, SHFOLDERCUSTOMSETTINGS, 4*2 + A_PtrSize*8)
   NumPut(iconIndex, SHFOLDERCUSTOMSETTINGS, 4*2 + A_PtrSize*9 + 4)
   DllCall("Shell32\SHGetSetFolderCustomSettings", Ptr, &SHFOLDERCUSTOMSETTINGS, WStr, folderPath, UInt, FCS_FORCEWRITE)
}

;GimpThumbGenerate(File="",Position="") {
;
;}

Base64Enc(ByRef Bin,nBytes,LineLength:=5120000,LeadingSpaces:=0) { ; By SKAN / 18-Aug-2017
	Local Rqd := 0, B64, B := "", N := 0 - LineLength + 1, CRYPT_STRING_BASE64:= 0x1,NULL:=0
	DllCall("Crypt32.dll\CryptBinaryToString","Ptr",&Bin,"UInt",nBytes,"UInt",CRYPT_STRING_BASE64,"Ptr",NULL,"UIntP",Rqd)
	VarSetCapacity(B64,Rqd *(A_Isunicode? 2 : 1 ),0)
	DllCall("Crypt32.dll\CryptBinaryToString","Ptr",&Bin,"UInt",nBytes,"UInt",CRYPT_STRING_BASE64,"Str",B64,"UIntP",Rqd)
	B64:= StrReplace(B64:= StrReplace(B64,"="),"`r`n")
	Loop,% Ceil(StrLen(B64) /LineLength)
		B.= Format("{1:" LeadingSpaces "s}","" ) . SubStr(B64,N+=LineLength,LineLength)
	Return,RTrim(B,"`n")
}

onMsgbox(HookCr,eventcr,hWnd,idObject,idChild,dwEventThread) {
	winget,pid,pid,ahk_id %hwnd% 
	if(pid!=r_pid)
		return,	;if its our mbox change icon
	activ8(wparam="",lparam="",msg="",hwnd)
}

Hookinit:
HookMb:= dllcall("SetWinEventHook","Uint",0x0010,"Uint",0x0010,"Ptr",0,"Ptr"
, ProcMb_:= RegisterCallback("onMsgbox",""),"Uint",0,"Uint",0,"Uint",0x0000) ;WINEVENT_OUTOFCONTEXT:= 0x0000
return,

exit:
menu,tray,noicon
gosub,unhook
ExitApp,

unHook:
if(FileExist(TEMP_FILE))
	FileDelete,%TEMP_FILE%
else,sleep,300
dllcall("UnhookWinEvent","Ptr",ProcMb_)
sleep,20
dllcall("GlobalFree",    "Ptr",ProcMb_,"Ptr")
(%ProcMb_%):= ""
dllcall("UnhookWinEvent","Ptr",HookMb)
sleep,20
dllcall("GlobalFree",    "Ptr",HookMb,"Ptr")
(%HookMb%):= ""
return,



main() {

}

activ8(wparam="",lparam="",msg="",hwnd="") {
	local static smicon:= b64_2_hicon(smicon64)
	, lgicon:= b64_2_hicon(lgicon64)
	,large:=1, small:=0, m:= 0x80
	SendMessage,m,small,smicon,,ahk_id %hWnd% ;WM_SETICON,ICON_SMALL
	SendMessage,m,large,lgicon,,ahk_id %hWnd% ;WM_SETICON,ICON_LARGE
	Return,ErrorLevel
}

MenuTray:
mousegetpos,,,hwnd,CN
if(instr(CN,"ToolbarWindow")) {
	send,{RButton Up}
	Menu,Tray,Show
} return,

Menus:
menu,Tray,NoStandard
menu,Tray,Add,%	 splitpath(A_scriptFullPath).fn,% "do_nothing"
menu,Tray,disable,% splitpath(A_scriptFullPath).fn
menu,Tray,Add ,% "Open",%	"MenHandlr"
menu,Tray,Icon,% "Open",%	"C:\Icon\24\Gterminal_24_32.ico"
menu,Tray,Add ,% "Open Containing",%	"MenHandlr"
menu,Tray,Icon,% "Open Containing",%	"C:\Icon\24\explorer24.ico"
menu,Tray,Add ,% "Edit",%	"MenHandlr"
menu,Tray,Icon,% "Edit",%	"C:\Icon\24\explorer24.ico"
menu,Tray,Add ,% "Reload",%	"MenHandlr"
menu,Tray,Icon,% "Reload",%	"C:\Icon\24\eaa.bmp"
menu,Tray,Add,%	 "Suspend",%	"MenHandlr"
menu,Tray,Icon,% "Suspend",%	"C:\Icon\24\head_fk_a_24_c1.ico"
menu,Tray,Add,%	 "Pause",%		"MenHandlr"
menu,Tray,Icon,% "Pause",%		"C:\Icon\24\head_fk_a_24_c2b.ico"
menu,Tray,Add ,% "Exit",%		"MenHandlr"
menu,Tray,Icon,% "Exit",%		"C:\Icon\24\head_fk_a_24_c2b.ico"

;msgb0x((ahkexe:= splitpath(A_AhkPath)).fn
;,	 (_:= (splitpath(A_scriptFullPath).fn) " Started`n@ " time4mat() "   In:  "
;.	_:= (a_tickCount-a_scriptStartTime) " Ms"),3) ;sleep,100 ;_:=""

a_scriptStartTime:= time4mat(a_now,"H:m - d\M")
menu,Tray,Tip,% splitpath(A_scriptFullPath).fn "`nRunning, Started @`n" a_scriptStartTime
do_nothing:
return,

MenHandlr(isTarget="") {
	listlines,off
	switch,(isTarget=""? a_thismenuitem : isTarget) {
		case,"Open Containing": TT("Opening "   a_scriptdir "..." Open_Containing(A_scriptFullPath),1)
		case,"edit","Open","SUSPEND","pAUSE":
			PostMessage,0x0111,(%a_thismenuitem%),,,% A_ScriptName " - AutoHotkey"
		case,"RELOAD": reload()
		case,"EXIT": exitapp
		default: isLabel(a_thismenuitem)? timer(a_thismenuitem,-10) : ()
	}	return,1
}

AHK_NOTIFYICON(byref wParam="", byref lParam="") {
	listlines,off
	switch,lParam {
		case,0x0204: settimer,MenuTray,-10 ;WM_RBUTTONdn RBD Will initiate the menu RBU will select item
		case,0x0203: 	PostMessage,0x0111,%open%,,,% A_ScriptName " - AutoHotkey"
			sleep(80),tt("Loading...","tray",1) ; WM_LBdoubleclick
	}	return,
}

reload() {
	reload,
	exitapp,
}

Varz:
global r_pid,	smicon64, lgicon64, EDIT:=65304, open:=65407, Suspend:=65305, PAUSE:=65306, exit:=6530
,	r_pid:= DllCall("GetCurrentProcessId")
smicon64:="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"
lgicon64:="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"
FolderBack64:="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"
folderfront64:="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"
return,