' ***************************************************************************
'                  <<<  Iron Maiden Legacy of the Beast  >>>
'                      brought to you by Team Hackjob
'   		based on the original table - Mago de Oz by JPSalas
'
'     A huge THANK YOU to the fantastic JP Salas for his work and talent
' ***************************************************************************
' ***************************************************************************
'                       	VISUAL PINBALL X (10.7.2+)
'		             		 Iron Maided LOTB Script
'						 	  By Team Hackjob + VPW
'                               Version 1.0.11
'
'   Without the following people this could never happen.  Special thanks to: 
'		RetroG33K	- Initial developement, playfield, assets and script
'		Soundscape 	- Initial developement, playfield, assets and script
'		Iaaki		- All around dev support 
'		Daphishbowl - Updated script and Pup Interaction
'		Apophis		- Lighting, physics, models and testing
'		Benji		- Playfield Support 
'		Hawkeye		- Awesome server & storage space 
'		Sixtoe		- Lighting, Models, Physics, VR Cabinet
'		MrH			- Models 
'		Rik			- Rules, gameply and playfield support with massive testing 
'		Nailbuster	- Pup support
' 		Tomate		- Awesome modeling
'		AstroNasty	- Playfield Images, debugging and testing
'		TerryRed	- Everything support 
'		DarkStar 	- Table support
'		HiRez00		- Audio enhancments
'		Seirif		- Massive Table Support 
'		Rothbauerw  - Awesome code for better physics and object interactions 
'		PinStratsDan - Awesome Testing 
'		leojreimroc  - VR Addition
'       DigitalJedi  -DOF 
'
' ***************************************************************************
'
'                          <<< IMPORTANT NOTES >>>
'
'	Visual Pinball X (10.7.1 Rev154) or later is required to play this table  <---  READ THIS AND FOLLOW!!!
'
'   No original music is distributed with this table, only MIDI equivilents 
'   	To add the table music Open the file PUPVideos\IM_LOTB\Music\WhereAreTheSongs-README.txt
'		Download the mp3 version of each song and rename to the filename before each link and save to the Music directory
'
'****************************************************************************

Option Explicit
SetLocale 1033					' Force US format so math works out


'*************************** ----General Options---- ******************************
Const bUsePlungerForSternKey 	= False	' Defaults to Right Magna Button but you can use Plunger button also	
Const kBallSearchEnabled 		= True	'  Do ball search if ball gets lost 
Const kBallSearchTimeout 		= 20000	'  Start ball search after 18 seconds of no activity
Const FreePlay					= False	' Coins or not
Const Music8Bit					= True  ' Use 8Bit version of Music (View README.txt in pup music dir to get original songs)
Const HasRealTiltBob			= 0 	' 0=No Tilt Bob, 1=Real TiltBob
Const AttractSilent	= 0					' 0 = audio and DOF played during attract sequence, 1 = no audio and no DOF during attract sequence(after first play)
Const kMaxSongs=14						' To Add Songs Drop Song in the Music dir, add a new image to "SongSelection" dir and then update the UpdateDMDSong function.
Const StagedFlipperMod 			= 0 	' 0 = not staged, 1 - staged (dual leaf switches)
Const ColorizeModeInserts		= False	' Nice mod that colors the mode inserts 
Const OutlaneDifficulty			= 1		' 0=Hard, 1=Medium, 2=Easy : Moves outlane pegs
' Scorbit Variables:
Const ScorbitActive				= 0 	' Is Scorbit Active	
Const     ScorbitShowClaimQR		= 1 	' If Scorbit is active this will show a QR Code in the bottom left on ball 1 that allows player to claim the active player from the app
Const     ScorbitClaimSmall			= 0 	' Make Claim QR Code smaller for high res backglass 
Const     ScorbitUploadLog			= 1 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID		= 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)
Const bUseFlex=False					' Enable Flex DMD (Depricated - not tested)
Const KeepLogs=False 					' Set True to save debug log file (Testers Only)
'*************************** ----General Sound Options---- ******************************
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'*************************** ----VR Options---- ******************************
Const VRRoomChoice				= 1		' 1 = BaSti Room, 2 = Minimal Room

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' Dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "pup-pack_name"    ' name of the PuP-Pack / PuPVideos folder for this table

'**************************
'   PinUp Player USER Config - NO NEED TO CHANGE ANY OF THESE!!!
'**************************
dim PuPDMDDriverType: PuPDMDDriverType=2   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
dim useRealDMDScale : useRealDMDScale=1    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=true    ' true or false to use DMD splash videos.
Dim pGameName       : pGameName="IM_LOTB"  'pupvideos foldername, probably set to cGameName in realworld
Const 			    TableName = "IM_LOTB"

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub PuPEvent(EventNum)
WriteToLog "Pup", "PupEvent: " & EventNum
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

Sub PuPEventD(EventNum)
WriteToLog "Pup", "PupEvent: " & EventNum
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "D"&EventNum,1  'send event to Pup-Pack
End Sub

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack
Randomize

Const BallSize = 50       ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1       ' standard mass (needed for nFozzy physics to work properly)
Const SongVolume = 0.3   ' 1 is full volume.


'***********TABLE VOLUME LEVELS ********* 
' [Value is from 0 to 1 where 1 is full volume. 
' NOTE: you can go past 1 to amplify sounds]
Const cVolBGVideo = 0.8 ' Volume for Video Clips   
Const cVolBGMusic = 0.7	' Volume for Music 
Const cVolDef 	= 0.2	' Default volume for callouts (Voice callouts)
Const cVolSfx 	= 0.3	' Volume for Game Sound effects  (Hitting targets, ramps, etc)
Const cVolTable = 1		' Volume for table Sound effects (Kickers, flippers, bumpers, etc)
' NOTE: See MECHANICAL SOUNDS section of code


' SERVICE Key Definitions
Const ServiceCancelKey 	= 8			' 7 key
Const ServiceUpKey 		= 9			' 8 key
Const ServiceDownKey 	= 10		' 9 key
Const ServiceEnterKey 	= 11		' 0 key


' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    ExecuteGlobal GetTextFile("VPMKeys.vbs")
    If Err Then MsgBox "Can't open VPMKeys.vbs"
    On Error Goto 0
End Sub


' Define any Constants
Const cGameName = "IM_LOTB"
Const myVersion = "1.0.12"
Const MaxPlayers = 4          ' from 1 to 4
Const MaxMultiplier = 3       ' limit playfield multiplier
Const MaxBonusMultiplier = 50 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 6       ' max number of balls during multiballs
Dim BallSaverTime             ' 20 in seconds

' Use FlexDMD if in FS mode
Dim UseFlexDMD
if bUseFlex then 
	If Table1.ShowDT = True then
		UseFlexDMD = False
	Else
		UseFlexDMD = True
	End If
Else	
	UseFlexDMD=False
End if 

' VRRoom set based on RenderingMode
' Internal DMD in Desktop Mode, using a textbox (must be called before LoadVPM)
Dim UseVPMDMD, VRRoom, DesktopMode
If RenderingMode = 2 Then VRRoom = VRRoomChoice Else VRRoom = 0


Dim pupColorBrown:pupColorBrown=RGB(132,89,39)
Dim pupColorGrey:pupColorGrey=RGB(200,197,200)
Dim pupColorGreen:pupColorGreen=RGB(23,237,18)
Dim pupColorBlack:pupColorBlack=RGB(0,0,0)
Dim pupColorWhite:pupColorWhite=RGB(255,255,255)
Dim pupColorYellow:pupColorYellow=RGB(223,220,40)
Dim pupColorRed:pupColorRed=RGB(245, 8, 8)				' RGB(247, 170, 51)??
Dim pupColorBlue:pupColorBlue=RGB(14,23,212)			' RGB(69, 93, 218)
' ***********************************************************************
' DAPHISHBOWL - STERN Service Menu
' ***********************************************************************
Dim serviceSaveAttract
dim serviceIdx
Dim serviceLevel
dim bServiceMenu
dim bServiceVol
dim IdxRef
dim ArrayRef
dim bServiceEdit
dim serviceOrigValue
Dim bInService:bInService=False
Dim MasterVol:MasterVol=100
Dim VolBGVideo:VolBGVideo = cVolBGVideo
Dim VolBGMusic :VolBGMusic = cVolBGMusic
Dim VolDef   ' Voice callouts
VolDef = cVolDef		
Dim VolSfx:VolSfx = cVolSfx
Dim VolTable:VolTable = cVolTable
Dim TopArray:TopArray = Array("GO TO DIAGNOSTIC MENU","GO TO AUDITS MENU","GO TO ADJUSTMENTS MENU","GO TO UTILITIES MENU","GO TO TOURNAMENT MENU","GO TO REDEMPTION MENU","EXIT SERVICE MENU")
Dim AdjArray:AdjArray = Array("STANDARD ADJUSTMENTS","FEATURE ADJUSTMENTS","PREVIOUS MENU","EXIT SRVICE MENU","HELP")
Const kMenuNone 	= -1
Const kMenuTop	 	= 0
Const kMenuAdj	 	= 1
Const kMenuAdjStd	= 2
Const kMenuAdjFeat	= 3
Sub StartServiceMenu(keycode)
	Dim i
	Dim maxVal
	Dim minVal
	Dim dataType
	dim TmpStr
	Dim direction
	Dim values
	Dim valuesLst
	Dim NewVal
	Dim displayText
	
	if keycode=ServiceEnterKey and bInService=False then 
'WriteToLog "     ", "Start Service:" & bInService
		PlaySoundVol "stern-svc-enter", VolSfx
		serviceSaveAttract=pInAttract
		pInAttract=False
		bServiceMenu=False
		bServiceEdit=False 
		bInService=True
		bServiceVol=False
		serviceIdx=-1
		serviceLevel=kMenuNone
		PuPlayer.LabelShowPage pDMDFull, 2, 0,""		' Show Blank Page so it doesnt overlap (We shouldnt need this but labels are popping over)
		PuPlayer.LabelShowPage pOverVid, 3,0,""
'			PuPlayer.playlistplayex pBackglass, "PuPOverlays" ,"3_Multicolor_TVS.png",  1, 1

		for i = 0 to 50 
			PuPlayer.LabelSet pOverVid,"ServiceV"&i,"\\PupOverlays\\clear.png" 	,1,"{'mt':2,'height':10,'width':2,'ypos':45,'xpos':"&i*2&"}"
		Next

		' If we are in attract 
		PuPlayer.playlistplayex pOverVid,"PupOverlays","clear1.png", 1, 1
		PuPlayer.PlayStop pOverVid
		PuPlayer.playlistplayex pOverVid,"Callouts","blank.mp4", 1, 1
		PuPlayer.SetLoop pOverVid, 1

		PuPlayer.LabelSet pOverVid,"ServiceL1","IRON MAIDEN" 												,1,"{'mt':2,'ypos':25,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL2","V" & myVersion & "    SYS. 2.35.0    OS. 2.00.16" 		,1,"{'mt':2,'ypos':35,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL3","" 														,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL3B","SERVICE MENU" 											,1,"{'mt':2,'ypos':43,'size':14}"
		PuPlayer.LabelSet pOverVid,"ServiceL4","PRESS 'SELECT' TO CONTINUE" 							,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\clear.png", 1,"{'mt':2,'ypos':25,'width':90}"
	elseif keycode=ServiceCancelKey then 					' 7 key starts the service menu
		if bInService then
			PlaySoundVol "stern-svc-cancel", VolSfx
			Select case serviceLevel:
				case kMenuTop, kMenuNone: 
					bInService=False
					bServiceVol=False
					pInAttract=serviceSaveAttract
					if pInAttract then
						PuPlayer.LabelShowPage pDMDFull,4,0,""
						pAttractStart(False)
					else 
						PuPlayer.LabelShowPage pDMDFull, 1, 0,""		' Show Blank Page so it doesnt overlap (We shouldnt need this but labels are popping over)
					End if 
					PuPlayer.LabelShowPage pOverVid, 1,0,""
					PuPlayer.PlayStop pOverVid
					PuPlayer.SetLoop pOverVid, 0

				case kMenuAdj:
					serviceLevel=kMenuNone
					serviceIdx=0
					StartServiceMenu 11
				case kMenuAdjStd,kMenuAdjFeat:
					if bServiceEdit then 
						bServiceEdit=False 
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							DMDStd(DMDMenu(serviceIdx).StdIdx)=serviceOrigValue
							PuPlayer.LabelSet pOverVid,"ServiceL3",DMDStd(DMDMenu(serviceIdx).StdIdx), 1,"{'mt':1,'at':1,'fq':150,'len':1}"
						else 
							DMDStd(DMDMenu(serviceIdx).FeatureIdx)=serviceOrigValue
							PuPlayer.LabelSet pOverVid,"ServiceL3",DMDStd(DMDMenu(serviceIdx).FeatureIdx), 1,"{'mt':1,'at':1,'fq':150,'len':1}"
						End if 
					else 
						PuPlayer.LabelSet pOverVid,"ServiceL1",""						 									,1,"{'mt':2,'ypos':65,'size':5}"
						PuPlayer.LabelSet pOverVid,"ServiceL2","" 															,1,"{'mt':2,'ypos':65,'size':5}"
						PuPlayer.LabelSet pOverVid,"ServiceL3",""															,1,"{'mt':2,'ypos':65,'size':14}"
						PuPlayer.LabelSet pOverVid,"ServiceL3B",""															,1,"{'mt':2,'ypos':65,'size':14}"
						PuPlayer.LabelSet pOverVid,"ServiceL4",""															,1,"{'mt':2,'ypos':65,'size':5}"
						serviceLevel=kMenuTop
						serviceIdx=2
						StartServiceMenu 11
					End if 
			End Select  
		End if 
	elseif bInService Then
		if keycode=ServiceEnterKey then 	' Select 
			PlaySoundVol "stern-svc-enter", VolSfx
			select case serviceLevel
				case kMenuNone:
					serviceLevel=kMenuTop
					serviceIdx=0
					PuPlayer.LabelSet pOverVid,"ServiceL1","", 1,"{'mt':2,'ypos':65,'size':5}"
					PuPlayer.LabelSet pOverVid,"ServiceL2","", 1,"{'mt':2,'ypos':65,'size':5}"
					PuPlayer.LabelSet pOverVid,"ServiceL3","", 1,"{'mt':2,'ypos':65,'size':5}"
					PuPlayer.LabelSet pOverVid,"ServiceL3B","", 1,"{'mt':2,'ypos':65,'size':14}"
					PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\DMDSettings1.png", 1,"{'mt':2,'ypos':25,'width':90, 'height':30}"
					PuPlayer.LabelSet pOverVid,"ServiceL4","GO TO DIAGNOSTIC MENU", 1,"{'mt':2,'ypos':65,'size':5}"
					' DIAGNOSTIC, AUDITS,  ADJUSTMENTS, UTILITIES, TOURNAMENT, REDEMPTION, EXIT  SERVICE MENU
					' STANDARD ADJUSTMENTS, FEATURE ADJUSTMENTS
				case kMenuTop: 
					if serviceIdx=2 then 
						serviceLevel=kMenuAdj
						serviceIdx=0
						PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\DMDSettings8.png", 1,"{'mt':2,'ypos':25,'width':90}"
						PuPlayer.LabelSet pOverVid,"ServiceL4",AdjArray(serviceIdx), 1,"{'mt':2,'ypos':65,'size':5}"
					elseif serviceIdx=6 then 
						StartServiceMenu 8		' Exit 
						Exit sub 
					else 
						PlaySoundVol "sfx-deny", VolSfx
					End if 
				case kMenuAdj:
					if serviceIdx=0 then 
						serviceLevel=kMenuAdjStd
						serviceIdx=0
						StartServiceMenu 0
					elseif serviceIdx=1 then
						serviceLevel=kMenuAdjFeat
						serviceIdx=11					' <-- Start of the Feature Adjustments
						StartServiceMenu 0
						Exit sub 
					elseif serviceIdx=2 then 		' Go Up
						serviceLevel=kMenuNone
						serviceIdx=0
						StartServiceMenu kFeatureMenuStart
						Exit sub 
					elseif serviceIdx=3 then 
						StartServiceMenu 8		' Exit 
						Exit sub 
					else 
						PlaySoundVol "sfx-deny", VolSfx
					End if
				case kMenuAdjStd, kMenuAdjFeat:
					if DMDMenu(serviceIdx).ValType="FUN" then		' Function
						if DMDMenu(serviceIdx).FeatureIdx=0 then 
							Clearhs
							PuPlayer.LabelSet pOverVid,"ServiceL3","<DONE>", 1,"{'mt':2,'ypos':45,'size':5}"
						End if 
						if DMDMenu(serviceIdx).FeatureIdx=1 then 
							ClearAll
							PuPlayer.LabelSet pOverVid,"ServiceL3","<DONE>", 1,"{'mt':2,'ypos':45,'size':5}"
						End if 
					else 
						if bServiceEdit=False then 	' Start Editing  
							bServiceEdit=True
							if DMDMenu(serviceIdx).StdIdx <> -1 then 
								serviceOrigValue=DMDStd(DMDMenu(serviceIdx).StdIdx)
							else 
								serviceOrigValue=DMDStd(DMDMenu(serviceIdx).FeatureIdx)
							End if
							PuPlayer.LabelSet pOverVid,"ServiceL3",serviceOrigValue, 1,"{'mt':1,'at':1,'fq':150,'len':20000}"
						else 							' Save the change 
							bServiceEdit=False
							if DMDMenu(serviceIdx).StdIdx <> -1 then 
								PuPlayer.LabelSet pOverVid,"ServiceL3",DMDStd(DMDMenu(serviceIdx).StdIdx), 1,"{'mt':1,'at':1,'fq':150,'len':1}"
							else 
								PuPlayer.LabelSet pOverVid,"ServiceL3",DMDStd(DMDMenu(serviceIdx).FeatureIdx), 1,"{'mt':1,'at':1,'fq':150,'len':1}"
							End if 
						End if 
					End if 
			End Select 
		elseif serviceLevel<>kMenuNone then 
			if bServiceEdit = False then 
				if keycode=ServiceUpKey then 	' Left
					PlaySoundVol "stern-svc-minus", VolSfx
					serviceIdx=serviceIdx-1
					if serviceIdx<0 then serviceIdx=0
				elseif keycode=ServiceDownKey then 	' Right 
					PlaySoundVol "stern-svc-plus", VolSfx
					serviceIdx=serviceIdx+1
				End if

				select case serviceLevel
					case kMenuTop:
						if serviceIdx>6 then serviceIdx=0
						PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\DMDSettings" & serviceIdx+1 &".png", 1,"{'mt':2,'ypos':25,'width':90}"
						PuPlayer.LabelSet pOverVid,"ServiceL4",TopArray(serviceIdx), 1,"{'mt':2,'ypos':65,'size':5}"

					case kMenuAdj:
						if serviceIdx>4 then serviceIdx=0
						PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\DMDSettings" & serviceIdx+8 &".png", 1,"{'mt':2,'ypos':25,'width':90}"
						PuPlayer.LabelSet pOverVid,"ServiceL4",AdjArray(serviceIdx), 1,"{'mt':2,'ypos':65,'size':5}"

					case kMenuAdjStd,kMenuAdjFeat:
						if serviceLevel=kMenuAdjStd and serviceIdx>=kFeatureMenuStart then serviceIdx=kFeatureMenuStart-1
						if serviceLevel=kMenuAdjFeat and serviceIdx<kFeatureMenuStart then serviceIdx=kFeatureMenuStart
						if serviceIdx>ubound(DMDMenu)-1 then serviceIdx=ubound(DMDMenu)-1
						PuPlayer.LabelSet pOverVid,"ServiceL2",DMDMenu(serviceIdx).Name, 1,"{'mt':2,'ypos':35,'size':5}"
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							PuPlayer.LabelSet pOverVid,"ServiceL1", "STANDARD ADJUSTMENT #" & serviceIdx & "(" & DMDMenu(serviceIdx).StdIdx & ")"  , 1,"{'mt':2,'ypos':25,'size':5}"
							PuPlayer.LabelSet pOverVid,"ServiceL3",DMDStd(DMDMenu(serviceIdx).StdIdx), 1,"{'mt':2,'ypos':45,'size':5}"
						elseif DMDMenu(serviceIdx).ValType<>"FUN" then 
							PuPlayer.LabelSet pOverVid,"ServiceL1", "GAME ADJUSTMENT #" & serviceIdx & "(" & DMDMenu(serviceIdx).FeatureIdx & ")", 1,"{'mt':2,'ypos':25,'size':5}"
							PuPlayer.LabelSet pOverVid,"ServiceL3",DMDFet(DMDMenu(serviceIdx).FeatureIdx), 1,"{'mt':2,'ypos':45,'size':5}"
						else 
							PuPlayer.LabelSet pOverVid,"ServiceL1", "GAME FUNCTION #" & serviceIdx , 1,"{'mt':2,'ypos':25,'size':5}"
							PuPlayer.LabelSet pOverVid,"ServiceL3","<EXECUTE>", 1,"{'mt':2,'ypos':45,'size':5}"
						End if 
						PuPlayer.LabelSet pOverVid,"ServiceL3B","", 1,"{'mt':2,'ypos':65,'size':14}"
						PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\clear.png", 1,"{'mt':2,'ypos':25,'width':90}"
						PuPlayer.LabelSet pOverVid,"ServiceL4","", 1,"{'mt':2,'ypos':65,'size':5}"		
				End Select
			else 	' ******************************************************    HANDLE EDIT MODE
				dataType=mid(DMDMenu(serviceIdx).ValType, 1, 3)
				minVal=-1
				maxVal=-1
				if Instr(DMDMenu(serviceIdx).ValType, "[")<> 0 then 
					TmpStr =mid(DMDMenu(serviceIdx).ValType, 5, Len(DMDMenu(serviceIdx).ValType)-5)
					values = Split(TmpStr, ":")
					if dataType="INT" or dataType="PCT" then 
						minVal=CLNG(values(0))
						maxVal=CLNG(values(1))
					End if 
				End if

				direction=0
				if keycode=ServiceUpKey then 	' Left
					PlaySoundVol "stern-svc-minus", VolSfx
					direction=-1
				elseif keycode=ServiceDownKey then 	' Right 
					PlaySoundVol "stern-svc-plus", VolSfx
					direction=1
				End if
'WriteToLog "     ", "EDIT DT:" & dataType & " min:" & minVal & " Max:" & maxVal & " dir:" & direction
				if direction<>0 then 
					if dataType="INT" or dataType="PCT" then 
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							NewVal=DMDStd(DMDMenu(serviceIdx).StdIdx) + (direction*DMDMenu(serviceIdx).Increment)
							if minVal=-1 or (NewVal <= maxVal and NewVal >= minVal) then 
								DMDStd(DMDMenu(serviceIdx).StdIdx)=DMDStd(DMDMenu(serviceIdx).StdIdx) + (direction*DMDMenu(serviceIdx).Increment)
								SaveValue TableName, "DMDStd_"&DMDMenu(serviceIdx).StdIdx, DMDStd(DMDMenu(serviceIdx).StdIdx)	' SAVE 
								if DMDMenu(serviceIdx).StdIdx<>kDMDStd_Initials and DMDMenu(serviceIdx).StdIdx<>kDMDStd_LeftStartReset then 
									SaveValue TableName, "dmdCriticalChanged", "True"		' SAVE
									dmdCriticalChanged=True
								End if 
							end if 
							displayText=DMDStd(DMDMenu(serviceIdx).StdIdx)
						else 
							NewVal=DMDFet(DMDMenu(serviceIdx).FeatureIdx) + (direction*DMDMenu(serviceIdx).Increment)
							if minVal=-1 or (NewVal <= maxVal and NewVal >= minVal) then 
								DMDFet(DMDMenu(serviceIdx).FeatureIdx)=DMDStd(DMDMenu(serviceIdx).FeatureIdx) + (direction*DMDMenu(serviceIdx).Increment)
								SaveValue TableName, "DMDFet_"&DMDMenu(serviceIdx).FeatureIdx, DMDFet(DMDMenu(serviceIdx).FeatureIdx)	' SAVE 
								SaveValue TableName, "dmdCriticalChanged", "True"		' SAVE
								dmdCriticalChanged=True
							End if 
							displayText=DMDFet(DMDMenu(serviceIdx).FeatureIdx)
						End if
					elseif dataType="BOL" then 
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							IdxRef=DMDMenu(serviceIdx).StdIdx 
							ArrayRef=0
						Else 
							IdxRef=DMDMenu(serviceIdx).FeatureIdx
							ArrayRef=1
						End if 
						
						if GetDMDVal(ArrayRef, IdxRef) = False then
							SetDMDVal ArrayRef, IdxRef, True
							SaveDMDVal ArrayRef, IdxRef
							displayText="True"
						elseif GetDMDVal(ArrayRef, IdxRef)=True then 
							SetDMDVal ArrayRef, IdxRef, False
							SaveDMDVal ArrayRef, IdxRef
							displayText="False"
						End if 
					elseif dataType="LST" then 
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							For i = 0 to ubound(values)
								valuesLst=Split(values(i), ",")
'WriteToLog "     ", "EDIT LST:" & valuesLst(0) & " " & valuesLst(1) & " val: " & DMDStd(DMDMenu(serviceIdx).StdIdx) & " idx:" & i & " ubound:" & ubound(values)
								if DMDStd(DMDMenu(serviceIdx).StdIdx)&"" = valuesLst(0) and i <> ubound(values) and direction=1 then 
									valuesLst=Split(values(i+1), ",")
									DMDStd(DMDMenu(serviceIdx).StdIdx)=valuesLst(0)
									SaveValue TableName, "DMDStd_"&DMDMenu(serviceIdx).StdIdx, DMDStd(DMDMenu(serviceIdx).StdIdx)	' SAVE 

									displayText=valuesLst(1)
									Exit For
								elseif DMDStd(DMDMenu(serviceIdx).StdIdx)&"" = valuesLst(0) and i <> lbound(values) and direction=-1 then 
									valuesLst=Split(values(i-1), ",")
									DMDStd(DMDMenu(serviceIdx).StdIdx)=valuesLst(0)
									SaveValue TableName, "DMDStd_"&DMDMenu(serviceIdx).StdIdx, DMDStd(DMDMenu(serviceIdx).StdIdx)	' SAVE 

									displayText=valuesLst(1)
									Exit For
								End if 
							Next 
						End if 
					End if 

					if displayText<>"" then 
						if DMDMenu(serviceIdx).StdIdx <> -1 then 
							PuPlayer.LabelSet pOverVid,"ServiceL3",displayText, 1,"{'mt':1,'at':1,'fq':150,'len':20000}"
						else 
							PuPlayer.LabelSet pOverVid,"ServiceL3",displayText, 1,"{'mt':1,'at':1,'fq':150,'len':20000}"
						End if
					End if 
				End if 
				' TBD implement edit here 
			End if 
		End if
	elseif keycode=ServiceUpKey or keycode=ServiceDownKey then 		' If you press 8 & 9 without being in service you do volume 
		if keycode=ServiceUpKey and MasterVol>0 then 	' Left
			MasterVol=MasterVol-1
			PlaySoundVol "stern-svc-minus", VolSfx
		elseif keycode=ServiceDownKey and MasterVol<100 then 	' Right 
			PlaySoundVol "stern-svc-plus", VolSfx
			MasterVol=MasterVol+1
		End if

		VolBGVideo = cVolBGVideo * (MasterVol/100.0)
		VolBGMusic = cVolBGMusic * (MasterVol/100.0)
		VolDef 	 = cVolDef * (MasterVol/100.0)
		VolSfx 	 = cVolSfx * (MasterVol/100.0)

		if bServiceVol=False then
			bServiceVol=True 
			serviceSaveAttract=pInAttract
			pInAttract=False

			PuPlayer.LabelShowPage pDMDFull, 2, 0,""		' Show Blank Page so it doesnt overlap (We shouldnt need this but labels are popping over)
			PuPlayer.LabelShowPage pOverVid, 3,0,""

			' If we are in attract 
			PuPlayer.playlistplayex pOverVid,"PupOverlays","clear1.png", 1, 1
			PuPlayer.PlayStop pOverVid
			PuPlayer.playlistplayex pOverVid,"Callouts","blank.mp4", 1, 1
			PuPlayer.SetLoop pOverVid, 1
		End if 

		PuPlayer.LabelSet pOverVid,"ServiceL1",""						 									,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL2","" 															,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL4",""															,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL3B","VOLUME " & MasterVol 										,1,"{'mt':2,'ypos':25,'size':10}"
		for i = 0 to 50 
			if i<=(MasterVol/2) then
				PuPlayer.LabelSet pOverVid,"ServiceV"&i,"\\PupOverlays\\ServiceVol1.png" 	,1,"{'mt':2,'height':10,'width':2,'ypos':45,'xpos':"&i*2&"}"
			else 
				PuPlayer.LabelSet pOverVid,"ServiceV"&i,"\\PupOverlays\\ServiceVol2.png" 	,1,"{'mt':2,'height':10,'width':2,'ypos':45,'xpos':"&i*2&"}"
			End if 
		Next

		PuPlayer.LabelSet pOverVid,"ServiceL3","USE +/- TO ADJUST VOLUME" 									,1,"{'mt':2,'ypos':65,'size':5}"
		PuPlayer.LabelSet pOverVid,"ServiceL3Img","PupOverlays\\clear.png", 1,"{'mt':2,'ypos':25,'width':90}"

		tmrService.Enabled = False 
		tmrService.Interval = 5000
		tmrService.Enabled = True 
	End if 
End Sub

Sub tmrService_Timer()
	tmrService.Enabled = False 	

	bServiceVol=False 
	pInAttract=serviceSaveAttract
	if pInAttract then 
		pAttractStart(False)
	End if 
	PuPlayer.LabelShowPage pDMDFull, 2, 0,""		' Show Blank Page so it doesnt overlap (We shouldnt need this but labels are popping over)
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	PuPlayer.PlayStop pOverVid
	PuPlayer.SetLoop pOverVid, 0

End Sub 

Class DMDSettings
	Public Name 
	Public StdIdx
	Public FeatureIdx
	Public ValType					' bool, pct
	Public Increment			' value to increment by
	Public sub Setup(Name, StdIdx, FeatureIdx, ValType, Increment)
		me.name=name
		me.StdIdx=StdIdx
		me.FeatureIdx=FeatureIdx
		me.ValType=ValType
		me.Increment=Increment
	End sub 
End Class 
Const MaxDMDSetting = 23
Dim DMDMenu(23)				' MaxDMDSetting
Dim dmdChanged:dmdChanged=False							' Did we change a value 
Dim dmdCriticalChanged:dmdCriticalChanged=False			' Did we change a critical value 
Const kFeatureMenuStart = 12	


Const kDMDStd_Initials = &H69
Const kDMDStd_ExtraBallLimit = &H3B
Const kDMDStd_ExtraBallPCT = &H3C
Const kDMDStd_MatchPCT = &H3F
Const kDMDStd_TiltWarn = &H42
Const kDMDStd_TiltDebounce = &H43
Const kDMDStd_LeftStartReset = &H49
Const kDMDStd_BallSave = &H4C
Const kDMDStd_BallSaveExtend = &H4D
Const kDMDStd_ReplayType = &H2C
Const kDMDStd_ReplayPct = &H2D
Const kDMDStd_DynReplayStart = &H31

Const kDMDFet_FamilyMode = &H70
Const kDMDFet_MadnessEnabled = &HA5
Const kDMDFet_ReviveStartCnt = &H7C
Const kDMDFet_InactivityPause = &HBF
Const kDMDFet_TimerFearOfTheDark = 79
Const kDMDFet_TimerHallowed = 149
Const kDMDFet_TimerIcarus = 150
Const kDMDFet_TimeAcesBallSave = 151
Const kDMDFet_TimeRimeBallSave = 122
Sub DMDSettingsInit()
	Dim i
	Dim x

	DMDStd(kDMDStd_Initials)=3				' 10 Initials
	DMDStd(kDMDStd_ExtraBallLimit)=5		' 5
	DMDStd(kDMDStd_ExtraBallPCT)=1			' 1%   (Looks like default was 25% but doesnt seem right)
	DMDStd(kDMDStd_TiltWarn)=2				' 2 warnings 
	DMDStd(kDMDStd_TiltDebounce)=1000		' 1 second debounce
	DMDStd(kDMDStd_MatchPCT)=9				' 9%
	DMDStd(kDMDStd_LeftStartReset)=True		' Allow Left + Start to Reset the Game (True, False, FreePlay)
	DMDStd(kDMDStd_BallSave) = 5			' 5 second ball save 
	DMDStd(kDMDStd_BallSaveExtend) = 2000	' Time to pause ball save when triggers are hit (Custom)
	DMDStd(kDMDStd_ReplayType)=1			' Extra Game, Extra Ball
	DMDStd(kDMDStd_DynReplayStart)=125000000' Replay Value 
	DMDStd(kDMDStd_ReplayPct) = 10  		' Replay Percent

	DMDFet(kDMDFet_FamilyMode) = False		' Family Ball Save Scenes 
	DMDFet(kDMDFet_MadnessEnabled) = True	' If Can I Play With Madness Enabled 
	DMDFet(kDMDFet_ReviveStartCnt) = 3		' Revive Starting Letters
	DMDFet(kDMDFet_InactivityPause) = TRUE	' Inactivity to Pause Timers 

	DMDFet(kDMDFet_TimerFearOfTheDark) = 40	' Mode Timer 
	DMDFet(kDMDFet_TimerHallowed) = 40		' Mode Timer
	DMDFet(kDMDFet_TimerIcarus) = 40		' Mode Timer
	DMDFet(kDMDFet_TimeAcesBallSave) = 15	' Mode Ball Save Time
	DMDFet(kDMDFet_TimeRimeBallSave) = 15	' Mode Ball Save Time 

	' Load any Values from saved config 
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_Initials):		If(x <> "") Then DMDStd(kDMDStd_Initials)=x
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_ExtraBallLimit):	If(x <> "") Then DMDStd(kDMDStd_ExtraBallLimit)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_ExtraBallPCT):	If(x <> "") Then DMDStd(kDMDStd_ExtraBallPCT)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_TiltWarn):		If(x <> "") Then DMDStd(kDMDStd_TiltWarn)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_TiltDebounce):	If(x <> "") Then DMDStd(kDMDStd_TiltDebounce)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_MatchPCT):		If(x <> "") Then DMDStd(kDMDStd_MatchPCT)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_LeftStartReset):	If(x <> "") Then DMDStd(kDMDStd_LeftStartReset)=CBool(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_BallSave):		If(x <> "") Then DMDStd(kDMDStd_BallSave)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_BallSaveExtend):	If(x <> "") Then DMDStd(kDMDStd_BallSaveExtend)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_ReplayType):		If(x <> "") Then DMDStd(kDMDStd_ReplayType)=x
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_ReplayPct):		If(x <> "") Then DMDStd(kDMDStd_ReplayPct)=INT(x)
	x = LoadValue(TableName, "DMDStd_"&kDMDStd_DynReplayStart):	If(x <> "") Then DMDStd(kDMDStd_DynReplayStart)=INT(x)

	x = LoadValue(TableName, "DMDFet_"&kDMDFet_FamilyMode):			If(x <> "") Then DMDFet(kDMDFet_FamilyMode)=CBool(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_MadnessEnabled):		If(x <> "") Then DMDFet(kDMDFet_MadnessEnabled)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_ReviveStartCnt):		If(x <> "") Then DMDFet(kDMDFet_ReviveStartCnt)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_InactivityPause):	If(x <> "") Then DMDFet(kDMDFet_InactivityPause)=CBOOL(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_TimerFearOfTheDark):	If(x <> "") Then DMDFet(kDMDFet_TimerFearOfTheDark)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_TimerHallowed):		If(x <> "") Then DMDFet(kDMDFet_TimerHallowed)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_TimerIcarus):		If(x <> "") Then DMDFet(kDMDFet_TimerIcarus)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_TimeAcesBallSave):	If(x <> "") Then DMDFet(kDMDFet_TimeAcesBallSave)=INT(x)
	x = LoadValue(TableName, "DMDFet_"&kDMDFet_TimeRimeBallSave):	If(x <> "") Then DMDFet(kDMDFet_TimeRimeBallSave)=INT(x)
	x = LoadValue(TableName, "dmdCriticalChanged"):	If(x<>"") Then dmdCriticalChanged=True

	ReplayValue=DMDStd(kDMDStd_DynReplayStart)
	x = LoadValue(TableName, "ReplayValue"):	If(x<>"") Then ReplayValue=INT(x)

	BallSaverTime = DMDStd(kDMDStd_BallSave)

	For i = 0 to MaxDMDSetting-1
		set DMDMenu(i)=new DMDSettings
		select case i
			case 0:DMDMenu(i).Setup "HSTD INITIALS", 						kDMDStd_Initials	,	-1,		"LST[3,3 INITIALS:10,10 LETTER NAME]", 1 
			case 1:DMDMenu(i).Setup "EXTRA BALL LIMIT", 					kDMDStd_ExtraBallLimit,	-1,		"INT[0:9]", 1 
			case 2:DMDMenu(i).Setup "EXTRA BALL PERCENT", 					kDMDStd_ExtraBallPCT,	-1,		"PCT[0:50]", 1 
			case 3:DMDMenu(i).Setup "MATCH PERCENT", 						kDMDStd_MatchPCT,		-1,		"PCT[0:10]", 1 
			case 4:DMDMenu(i).Setup "LEFT+START RESETS", 					kDMDStd_LeftStartReset,	-1,		"BOL", 1 
			case 5:DMDMenu(i).Setup "BALL SAVE SECONDS", 					kDMDStd_BallSave,		-1,		"INT[0:15]", 1 
			case 6:DMDMenu(i).Setup "BALL SAVE EXTEND SEC", 				kDMDStd_BallSaveExtend,	-1,		"INT", 1 
			case 7:DMDMenu(i).Setup "TILT WARNINGS", 						kDMDStd_TiltWarn	,	-1,		"INT[0:3]", 1 
			case 8:DMDMenu(i).Setup "TILT DEBOUNCE", 						kDMDStd_TiltDebounce,	-1,		"INT[750:1500]", 1 
			case 9:DMDMenu(i).Setup "REPLAY TYPE", 							kDMDStd_ReplayType,		-1,		"LST[1,EXTRA GAME:2,EXTRA BALL]", 1 
			case 10:DMDMenu(i).Setup "DYNAMIC REPLAY START", 				kDMDStd_DynReplayStart,	-1,		"INT[60000000:150000000]", 1000000 
			case 11:DMDMenu(i).Setup "REPLAY PERCENTAGE", 					kDMDStd_ReplayPct,		-1,		"INT[1:50]", 1 
'kFeatureMenuStart
			case 12:DMDMenu(i).Setup  "FAMILY MODE", 						-1,		kDMDFet_FamilyMode,		"BOL", 1 
			case 13:DMDMenu(i).Setup "MADNESS MODE ENABLED", 				-1,		kDMDFet_MadnessEnabled,			"INT[1:5]", 1 
			case 14:DMDMenu(i).Setup "REVIVE STARTING LETTERS", 			-1,		kDMDFet_ReviveStartCnt,			"INT[0:5]", 1 
			case 15:DMDMenu(i).Setup "ALLOW INACTIVITY TO PAUSE TIMERS", 	-1,		kDMDFet_InactivityPause,		"BOL", 1
			case 16:DMDMenu(i).Setup "FEAR OF THE DARK TIMER",		 		-1,		kDMDFet_TimerFearOfTheDark,		"INT[20:120]", 1 
			case 17:DMDMenu(i).Setup "HALLOWED BE THY NAME TIMER",			-1,		kDMDFet_TimerHallowed,			"INT[20:120]", 1 
			case 18:DMDMenu(i).Setup "FLIGHT OF ICARUS TIMER", 				-1,		kDMDFet_TimerIcarus,			"INT[20:120]", 1 
			case 19:DMDMenu(i).Setup "ACES HIGH BALL SAVE TIME", 			-1,		kDMDFet_TimeAcesBallSave,		"INT[5:20]", 1 
			case 20:DMDMenu(i).Setup "RIME/MARINER BALL SAVE TIME", 		-1,		kDMDFet_TimeRimeBallSave,		"INT[5:20]", 1 
			case 21:DMDMenu(i).Setup "CLEAR HIGHSCORE", 					-1,		0,								"FUN", 1 
			case 22:DMDMenu(i).Setup "CLEAR ALL", 							-1,		1,								"FUN", 1 

		End Select 
	Next 

End Sub 
Dim DMDStd(120)
Dim DMDFet(293)
Public Function GetDMDVal(WhichArray, idx)
	if WhichArray=0 then 
		GetDMDVal=DMDStd(idx)
	Else 
		GetDMDVal=DMDFet(idx)
	End if 
End Function 
Public Sub SetDMDVal(WhichArray, idx, Value)
	if WhichArray=0 then 
		DMDStd(idx)=Value
	Else 
		DMDFet(idx)=Value
	End if 
End Sub 
Public Sub SaveDMDVal(WhichArray, idx)
	if WhichArray=0 then 
		SaveValue TableName, "DMDStd_"&idx, DMDStd(IdxRef)
	Else 
		SaveValue TableName, "DMDFet_"&idx, DMDFet(IdxRef)
	End if 
End Sub 

'  END DAPHISHBOWL - STERN DMD


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 2108 with your TableID from Scorbit 
'     Replace GRWvz-MP37P from your table on OPDB - eg: https://opdb.org/machines/2103
'		if Scorbit.DoInit(389, "PupOverlays", "1.0.0", "GRWvz-MP37P") then 
'			tmrScorbit.Interval=2000
'			tmrScorbit.UserValue = 0
'			tmrScorbit.Enabled=True 
'		End if 
' 3) Customize helper functions below for different events if you want or make your own 
' 4) Call 
'		StartSession - When a game starts 
'		StopSession - When the game is over
'		SendUpdate - called when Score Changes
'			SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
'			Example:  Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
'		SetGameMode - When different game events happen like starting a mode, MB etc.  (ScorbitBuildGameModes helper function shows you how)
' 5) Drop the binaries sQRCode.exe and sToken.exe in your Pup Root so we can create session tokens and QRCodes.
'	- Drop QRCode Images (QRCodeS.png, QRcodeB.png) in yur pup PuPOverlays if you want to use those 
' 6) Callbacks 
'		Scorbit_Paired   	- Called when machine is successfully paired.  Hide QRCode and play a sound 
'		Scorbit_PlayerClaimed	- Called when player is claimed.  Hide QRCode, play a sound and display name 
'
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
WriteToLog "Scorbit", "Scorbit PAIRED"
	PlaySoundVol "scorbit_detected_2", VolSfx
	PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\clear1.png",0,""
	PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\clear1.png",0,""
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
WriteToLog "Scorbit", "Scorbit LOGIN"
	PlaySoundVol "scorbit_login", VolSfx
	ScorbitClaimQR(False)
	puPlayer.LabelSet pDMDFull,"Player",	PlayerName	,1,""
'WriteToLog "     ", "Scorbit_PlayerClaimed:" & PlayerNum & " " & PlayerName
End Sub 


Sub ScorbitClaimQR(bShow)						'  Show QRCode on first ball for users to claim this position
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 

	if bShow and balls=1 and bGameInPlay and Scorbit.GetName(CurrentPlayer+1)="" then 
		if ScorbitClaimSmall=0 then ' Desktop Make it Larger
			PuPlayer.LabelSet pDMDFull, "ScorbitQR", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':20, 'height':40,'xalign':0,'yalign':0,'ypos':2,'xpos':75}"
			PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon", "PuPOverlays\\QRcodeB.png",1,"{'mt':2,'width':23, 'height':52,'xalign':0,'yalign':0,'ypos':0,'xpos':73.5,'zback':1}"
		else 
			PuPlayer.LabelSet pDMDFull, "ScorbitQR", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':12, 'height':24,'xalign':0,'yalign':0,'ypos':60,'xpos':5}"
			PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon", "PuPOverlays\\QRcodeB.png",1,"{'mt':2,'width':14, 'height':32.5,'xalign':0,'yalign':0,'ypos':58,'xpos':4,'zback':1}"
		End if 
	Else 
		PuPlayer.LabelSet pDMDFull, "ScorbitQR", "PuPOverlays\\clear1.png",0,""
		PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon", "PuPOverlays\\clear1.png",0,""
	End if 
End Sub 

Sub ScorbitBuildGameModes()		' Custom function to build the game modes for better stats 
	dim GameModeStr
	dim i
	if Scorbit.bSessionActive=False then Exit Sub 
	GameModeStr="NA:"

	if IsModeActive(kMode2M2M) then 
		GameModeStr="NA{red}:Two Minutes To Midnight"
	elseif IsModeActive(kModeIcarus) then 
		GameModeStr="NA{white}:Flight Of Icarus"
	elseif IsModeActive(kModeHallowed) then 
		GameModeStr="NA{orange}:Hallowed Be Thy Name"
	elseif IsModeActive(kModeRime) then 
		GameModeStr="NA{blue}:Rime of The Ancient Mariner"
	elseif IsModeActive(kModeFear) then 
		GameModeStr="NA{purple}:Fear Of The Dark"
	elseif IsModeActive(kModeAces) then 
		GameModeStr="NA{blue}:Aces High"
	End if 

	if IsModeQual(kModeMummy) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"MB{yellow}:Mummy Multiball"
	elseif IsModeQual(kModeCyborg) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"MB{red}:Cyborg Multiball"
	elseif IsModeQual(kModeTrooper) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"MB{green}:Trooper Multiball"
	elseif IsModeQual(kModeRTTH) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"WM{white}:Run To The Hills"
	elseif IsModeQual(kModeMadness) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"WM{red}:Can I Play With Madness"
	elseif IsModeQual(kModeNOTB) then 
		if GameModeStr<>"" then GameModeStr=GameModeStr & ";"
		GameModeStr=GameModeStr&"WM{purple}:Number Of The Beast"
	End if 

	Scorbit.SetGameMode(GameModeStr)

End Sub 

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			WriteToLog "Scorbit", "CREATING LOG"
		case 1:
			WriteToLog "Scorbit", "Uploading LOG"
		case 2:
			WriteToLog "Scorbit", "LOG Complete"
	End Select 
End Sub 
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE


dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()								' Timer to send heartbeat 
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub 
Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 
Class ScorbitIF

	Public bSessionActive
	Public bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LOGFILE(10000000)
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bRunAsynch
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayerNames(4)
	Private SaveCurrentPlayer

	Public bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False 
	End Sub 

	Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Sub DoTimer(bInterval)	' 2 second interval
		dim holdScores(4)
		dim i
		if bInterval=0 then 
			SendHeartbeat()
		elseif bRunAsynch then ' Game in play
			Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
		End if 
	End Sub 

	Function GetName(PlayerNum)	' Return Parsed Players name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayerNames(PlayerNum-1)
		End if 
	End Function 

	Function DoInit(MyMachineID, Directory_PupQRCode, Version, opdb)
		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		bRunAsynch=False 
		DoInit=False 
		opdbID=opdb
		dirQrCode=Directory_PupQRCode
		MachineID=MyMachineID
		TableVersion=version
		bNeedsPairing=False 
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				WriteToLog "Scorbit", "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
				MyMac=replace(Nad.MACAddress, ":", "")
				Exit For 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		WriteToLog "Scorbit", "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			WriteToLog "Scorbit", "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			MsgBox "SCORBIT - Can get UUID, Disabling."
			Exit Function
		elseif MyUUID="03000200-0400-0500-0006-000700080009" or ScorbitAlternateUUID then
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
'		WriteToLog "     ", "MyUUID:" & MyUUID 


' Debug
'		myUUID="adc12b19a3504453a7414e722f58737f"
'		Serial="123456778"

		' Authenticate and get our token 
		if getStoken() then 
			bEnabled=True 
'			SendHeartbeat
			DoInit=True
		End if 
	End Function 

	Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 

		if bWaitResp and objXmlHttpMain.readystate=4 then 
'			WriteToLog "     ", "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				WriteToLog "Scorbit", "RESPONSE: " & ResponseStr

				' Parse Name 
				if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
					if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
						Parts=Split(ResponseStr,",{")							' split it 
						if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
							if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
								CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
								CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
								Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
'								WriteToLog "     ", "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
							End if 
						End if
					End if 
				else												    ' Check for unclaim 
					if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
						Parts=Split(ResponseStr,"[")						' split it 
'WriteToLog "     ", "Parts:" & Parts(1)
						Parts2=Split(Parts(1),"}")							' split it 
						for i = 0 to Ubound(Parts2)
'WriteToLog "     ", "Parts2:" & Parts2(i)
							if instr(1, Parts2(i), """player"":null")<>0 Then
								CachedPlayerNames(i)=""
							End if 
						Next 
					End if 
				End if
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
WriteToLog "Scorbit", "Scorbit Start Session" 
		CachedPlayerNames(0)=""
		CachedPlayerNames(1)=""
		CachedPlayerNames(2)=""
		CachedPlayerNames(3)=""
		bRunAsynch=True 
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1
	End Sub 

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		if bSessionActive=False then Exit Sub 
WriteToLog "Scorbit", "Scorbit Stop Session" 

		bRunAsynch=False 
		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False													' Disable any more scores from being sent
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			WriteToLog "Scorbit", "Creating Scorbit Log: Size" & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LOGFILE(i)
			Next 
			objFile.Close
			LogIdx=0
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", puplayer.getroot&"\" & cGameName & "\sGameLog.csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			fso.DeleteFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			on error goto 0
		End if 

	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  CurrentPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if 

		if bEnabled=False then Exit Sub 
		if bWaitResp then exit sub ' Drop message until we get our next response 

		SaveCurrentPlayer=CurrentPlayer
'		PostData = "session_uuid=" & SessionUUID & "&session_time=" & DateDiff("S", "1/1/1970", Now()) & _
'					"&session_sequence=" & SessionSeq & "&active=" & bActive
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & CurrentPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode
		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		if resultStr<>"" then WriteToLog "Scorbit", "SendUpdate Resp:" & resultStr
	End Sub 

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = LCASE(Mid(TypeLib.Guid, 2, 36))		

'		Set wsh = CreateObject("WScript.Shell")
'		Set fso = CreateObject("Scripting.FileSystemObject")
'
'		dim rc
'		dim result
'		dim objFileToRead
'		Dim sessionID:sessionID=puplayer.getroot&"\" & cGameName & "\sessionID.txt"
'
'		on error resume next
'		fso.DeleteFile(sessionID)
'		On error goto 0 
'
'		rc = wsh.Run("powershell -Command ""(New-Guid).Guid"" | out-file -encoding ascii " & sessionID, 0, True)
'		if FileExists(sessionID) and rc=0 then
'			Set objFileToRead = fso.OpenTextFile(sessionID,1)
'			result = objFileToRead.ReadLine()
'			objFileToRead.Close
'			GUID=result
'		else 
'			MsgBox "Cant Create SessionUUID through powershell. Disabling Scorbit"
'			bEnabled=False 
'		End if

	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next 
		End if 
	End Function

	Private Sub SendHeartbeat()
		Dim resultStr
		dim TmpStr
		Dim Command
		Dim rc
		Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		WriteToLog "Scorbit", "Heartbeat Resp:" & resultStr
		If VenueMachineID="" then 

			if resultStr<>"" and Instr(resultStr, """unpaired"":true")=0 then 	' We Paired
				bNeedsPairing=False
				WriteToLog "Scorbit", "Paired"
				Scorbit_Paired()
			else 
				bNeedsPairing=True
			End if 

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'WriteToLog "Scorbit", "VenueMachineID=" & VenueMachineID			
				Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
			End if 
		End if 
	End Sub 

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
WriteToLog "Scorbit", "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
WriteToLog "Scorbit", "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing
'WriteToLog "Scorbit", result

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
WriteToLog "Scorbit", "Got TOKEN:" & sToken
				getStoken=True
			Else 
WriteToLog "Scorbit", "ERROR:" & result
				getStoken=False
			End if 
		else 
WriteToLog "Scorbit", "ERROR No File:" & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
WriteToLog "Scorbit", "Url:" & Url  & "  Async=" & bRunAsynch
		objXmlHttpMain.open "GET", Url, bRunAsynch
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				WriteToLog "Scorbit", "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
WriteToLog "Scorbit", "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
WriteToLog "Scorbit", "PostMSg:" & Url & " " & PostData

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				'WriteToLog "Scorbit", "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
WriteToLog "Scorbit", "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With
'		fso.Open sFileName For Binary Access Read As nFile
'		If LOF(nFile) > 0 Then
'			ReDim baBuffer(0 To LOF(nFile) - 1) As Byte
'			Get nFile, , baBuffer
'			sPostData = StrConv(baBuffer, vbUnicode)
'		End If
'		Close nFile

		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"

'WriteToLog "Scorbit", "POSTDATA: " & sPostData & vbcrlf

		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
WriteToLog "Scorbit", "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim ReplayValue
Dim bReplayAwarded(4)
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplierQual(4)
Dim PlayfieldMultiplier(4)
Dim PFxSeconds
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim ExtraBallsLit(4)
Dim LastScore(4)				' save off the last scores 
Dim Score(4)
Dim ScoreSave(4)				' Detect if score changed before updating
Dim HighScore(5)
Dim HighScoreName(5)
Dim StatName(31)
Dim StatScore(31)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim bResetCurrentGame
Dim Tilt
Dim TiltSensitivity
Dim TiltCount(4)
Dim Tilted
Dim BallSearchCnt
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim ticksFromLastEject
Dim SkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim bCreatedBall
Dim bAutoPlunger
Dim bInstantInfo
Dim bInstantInfoIdx
Dim bAttractMode
Dim x
Dim MotorCheck

' Define Game Control Variables
Dim bShatzEnabled
Dim LastSwitchHit
Dim LastSwitchHit2
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInRealLock
Dim bWaitMummyClear
Dim BallsFromSarcophagus
Dim BallsOutlaneDrainCnt
Dim BallsOutlaneDrainIgnoreCnt

Dim BallsInHole
Dim MusicDir

' Define Game Flags
Dim bFreePlay
Dim bShowMatch
Dim bGameInPlay
Dim bTableDisabled			' Disable table on Tilt/Reset
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverSkipAnimation
Dim bBallSaverActive
Dim bBallSaverGrace
Dim OutlaneBallsaveBuffer		' Allows outlanes to save a ball 
Dim bBallSaverReady
Dim bPlayfieldValidated
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bSuperSkillshotReady
Dim bSuperSSkillshotsReady(6)
Dim bSuperSSkillshotOrbits

Dim bExtraBallWonThisBall
Dim bPowerFeatureEBCollected(6)		' 6 super secret skill shots 
Dim bJustStarted
Dim bJackpot
Dim bSongSelect

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim cbRight   'captive ball Clown

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
	dim bShadow
    Randomize

    ' core.vbs definitions
    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 60 ' Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("", 141, DOFPulse, DOFContactors), SoundFXDOF("", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

	NewLog
	WriteToLog "-------------", "TABLE INIT"

'    Set cbRight = New cvpmCaptiveBall
'    With cbRight
'        .InitCaptive CapTrigger2, CapWall2, Array(CapKicker2, CapKicker2a), 0
'        .NailedBalls = 0
'        .ForceTrans = 2 ' .9
'        .MinForce = 3.5
'        .CreateEvents "cbRight"
'        .Start
'    End With

'    CapKicker2.CreateSizedBallWithMass BallSize / 2, BallMass
'    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass


	if Music8Bit then 
		MusicDir = "Music.8"
	else 
		MusicDir = "Music"
	End if 

	if ColorizeModeInserts then 
		SetLightColor lModeHallowed, orange, 0
		SetLightColor lModeRime, 	 cyan, 0
		SetLightColor lModeFear, 	 purple, 0
		SetLightColor lModeAces, 	 blue, 0
	End if 


    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs
	DMDSettingsInit()

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = FreePlay 'we want coins
	bShowMatch = False

    if bFreePlay Then DOF 125, DOFOn

	For i = 0 To MaxPlayers-1
		LastScore(i)=0
	Next 

	' Hide Ball shadows so they dont show up in VR
	For each bShadow in aBallShadow 
		bShadow.Visible=False
	Next 

	leftpeg_1.collidable = False 
	leftpeg_2.collidable = False 
	leftpeg_3.collidable = False 
	rightpeg_1.collidable = False 
	rightpeg_2.collidable = False 
	rightpeg_3.collidable = False 
	Select case OutlaneDifficulty
		case 0: ' hard
			leftpeg_1.collidable = True 
			rightpeg_1.collidable = True 
			OutlanePegL_gion.transz=11.5
			OutlanePegR_gion.transz=11.5
		case 1: ' medium
			leftpeg_2.collidable = True 
			rightpeg_2.collidable = True 
		case 2: ' easy 
			leftpeg_3.collidable = True 
			rightpeg_3.collidable = True 
			OutlanePegL_gion.transz=-12
			OutlanePegR_gion.transz=-12
	End Select 

    ' Init main variables and any other flags
	hsbModeActive=False 

	bResetCurrentGame=False
	bFirstPlay=True 
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
	bBallSaverGrace = True 
	OutlaneBallsaveBuffer = 0
	bBallSaverSkipAnimation = False 
    bBallSaverReady = False
	bPlayfieldValidated=False 
    bMultiBallMode = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
	BallsOutlaneDrainCnt=0
	BallsOutlaneDrainIgnoreCnt=0
	BallsInRealLock=0
	bWaitMummyClear=False 
	BallsFromSarcophagus=0
    BallsInLock(0) = 0
    BallsInLock(1) = 0
    BallsInLock(2) = 0
    BallsInLock(3) = 0
    BallsInHole = 0
	bShatzEnabled=False
    LastSwitchHit = ""
	LastSwitchHit2 = ""
    Tilt = 0
    TiltSensitivity = 6				
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
	bCreatedBall=False
    bInstantInfo = False
	bInstantInfoIdx = 0
    bSongSelect = False
    ' set any lights for the attract mode
    StartAttractMode(True)

    ' Start the RealTime timer
    RealTime.Enabled = 1
	
	pClearEverything

	tmrFlashGuardian.Interval = 150
	tmrFlashGuardian.Enabled = True

    ' Load table color
    LoadLut
End Sub

'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
	PuPlayer.B2SInit "", pGameName

	if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
		   PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
	End if

	PuPlayer.LabelInit pDMD


	if PuPDMDDriverType=pDMDTypeReal then
		Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
		PUPDMDObject.DMDOpen
		PUPDMDObject.DMDPuPMirror
		PUPDMDObject.DMDPuPTextMirror
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
	END IF


	pSetPageLayouts

	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	'pAttractStart			 ' firsttime running for like an startup video..

	if ScorbitActive then 
		if Scorbit.DoInit(2114, "PupOverlays", myVersion, "G4dOQ-MkPxr") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 

End Sub 'end PUPINIT


dim LockCurPos:LockCurPos=0
dim LockStopPos:LockStopPos=0
Dim LockCurInc:LockCurInc=1
Dim LockBall1
Dim LockBall2
Sub tmrBallLock_Timer()
'WriteToLog "     ", "LockCurPos:" & LockCurPos & " LockStopPos:" & LockStopPos & " inc:" & LockCurInc
	LockCurPos=LockCurPos+LockCurInc
	if (LockCurPos >= LockStopPos and LockCurInc>0) or (LockCurPos <= LockStopPos and LockCurInc<0) Then
		WriteToLog "     ", "tmrBallLock_Timer Done " & GameTime
		primBallLockTop3.Collidable=False 
		primBallLockTop2.Collidable=False 
		Select case LockStopPos
			Case -14:						' POS 0
				LockCurPos=-14
				RampLock0.Visible = True
				RampLock0.Collidable = True
				trgBallLock0.Enabled = True 
			case 0:							' POS 1
				LockCurPos=0
'				RampLock1.Visible = True 
				RampLock1.Collidable = True
				WallLock1.Collidable=True
				primBallLock.Collidable=True 			' Prevent balls from falling in when in this position
			case 8:							' POS 2
				LockCurPos=8
'				RampLock2.Visible = True 
				RampLock2.Collidable = True
				trgBallLock2.Enabled=True
				primBallLock.Collidable = False			' Need this non collidable so I can knock balls out 
				primBallLockTop2.Collidable=True 		' Prevent balls from falling in when in this position

			case 18:						' POS 3
				LockCurPos=18
'				RampLock3.Visible = True
				RampLock3.Collidable = True
				trgBallLock3.Enabled=True 
				primBallLockTop3.Collidable=True 		' Prevent balls from falling in when in this position
		End select 
		tmrBallLock.Enabled=False
	End if 

	bRotXInv=True
	DoRotateO primBallLockOrigin, primBallLock, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock1, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock2, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock3, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock4, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock5, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, primBallLock6, lockRad, LockCurPos, -180

	DoRotateO primBallLockOrigin, balltrapMetal_gion, lockRad, LockCurPos, -180
	DoRotateO primBallLockOrigin, balltrapPlastics_gion, lockRad, LockCurPos, -180
	

'if LockCurPos=-14 or LockCurPos=0 or LockCurPos=8 or LockCurPos=18 then 
'	WriteToLog "     ", "primBallLock3: Pos:" & LockCurPos & " z:" & primBallLock3.z & " y:" & primBallLock3.y & " z:" & primBallLock3.z & " rotx:" & primBallLock3.rotx
'End if 

	if BallsInRealLock>=1 then DoRotateO primBallLockOrigin, LockBall1, lock1Rad, LockCurPos, -180
'	if BallsInRealLock>=2 then DoRotateO primBallLockOrigin, LockBall2, lock2Rad, LockCurPos, -180

End Sub 


dim bRotXInv:bRotXInv=True
Dim lockRad:lockRad= SQR((primBallLockOrigin.Z - primBallLock.Z)^2 + (primBallLockOrigin.Y - primBallLock.Y)^2)
Sub DoRotateO(Primary, Secondary, SecondaryRad, Rot, RotOffset)
	if isNull(Secondary) then
		MsgBox "ERROR: Cant access locked ball: " & BallsInLock
		exit sub 
	End if 
	Secondary.Z = Primary.Z - SecondaryRad * sin ((Rot+RotOffset) * Pi/180)
	Secondary.Y = Primary.Y + SecondaryRad * cos ((Rot+RotOffset) * Pi/180)
	if typename(Secondary) <> "IBall" then 
		if bRotXInv then 
			Secondary.RotX = -Rot
		else 
			Secondary.RotX = Rot
		End if 
	Else
		Secondary.Z=Secondary.Z+25
		Secondary.velx = 0.05
		Secondary.vely = 0.05
		Secondary.velz = 0
	End If 
' WriteToLog "     ", "Rot (y, z):" & Secondary.Y & " " & Secondary.Z & " rad:" & SecondaryRad & " Rot:" & Rot
End Sub

Sub DoRotate(Primary, Secondary, SecondaryRad, Rot, RotOffset)
	dim y:y=SecondaryRad
	dim z:z=0
	rot=rot+RotOffset
	if isNull(Secondary) then
		MsgBox "ERROR: Cant access locked ball: " & BallsInLock
		exit sub 
	End if 
	Secondary.Y = Primary.Y + (y*cos(rot*Pi/180) - z*sin(rot*Pi/180))
	Secondary.Z = 26        + (y*sin(rot*Pi/180) - z*cos(rot*Pi/180))
'WriteToLog "     ", "Rot (y,z):" & Secondary.Y & " " & Secondary.Z
	if typename(Secondary) <> "IBall" then 
		Secondary.RotX = Rot
	Else
		Secondary.velx = 0.0
		Secondary.vely = 0.05
		Secondary.velz = 0.05
	End If 
End Sub

Dim gRampPos				' 0=Underworld, 1=Flat, 2=Eject, 3=RampLoad
gRampPos=15	
Sub RotateRamp(rampPos)		' 0=Underworld, 1=Flat, 2=Eject, 3=RampLoad
	RotateRamp2 rampPos, False
End Sub 

Sub RotateRamp2(rampPos, bMute)		' 0=Underworld, 1=Flat, 2=Eject, 3=RampLoad
WriteToLog "     ", "RotateRamp RAMPPOS:" & rampPos & " " & BallsInRealLock
	gRampPos=rampPos
	Select case rampPos
		Case 0:
'			Wall005_guide.collidable = False
			Wall005_guide.IsDropped = True
			CapKicker2a.Enabled = False
			CapKicker2a.Kick 0, 1
			OpenRamp(False)
			LockStopPos=-14
		case 1:
'			Wall005_guide.collidable = True 
			Wall005_guide.IsDropped = False
			CapKicker2a.Enabled = True
			OpenRamp(False)
			LockStopPos=0
		case 2:
'			Wall005_guide.collidable = True 
			Wall005_guide.IsDropped = False
			CapKicker2a.Enabled = True
			OpenRamp(False)
			LockStopPos=8
		case 3:
'			Wall005_guide.collidable = True 
			Wall005_guide.IsDropped = False
			CapKicker2a.Enabled = False
			OpenRamp(True)

			if BallsInRealLock>0 then 			' If a real ball is already locked just go to position 1 for a virtual lock 
				gRampPos=1
				LockStopPos=0
			Else 
				LockStopPos=18
			End if 
	End select

	trgBallLock0.Enabled=False
	trgBallLock2.Enabled=False
	trgBallLock3.Enabled=False 
'	RampLock0.Visible = False 
	RampLock0.Collidable = False 
	RampLock1.Visible = False 
	RampLock1.Collidable = False
	RampLock2.Visible = False 
	RampLock2.Collidable = False
	RampLock3.Visible = False 
	RampLock3.Collidable = False
	WallLock1.Collidable=False 
	if RampPos=1 then 
'		primBallLock.Collidable = True
	Else 
		primBallLock.Collidable = False
	End if

	if BallsInRealLock>=1 then lock1Rad= SQR((primBallLockOrigin.Z - LockBall1.Z)^2 + (primBallLockOrigin.Y - LockBall1.Y)^2)

	if LockStopPos<LockCurPos then 	' Select direction and speed
		LockCurInc=-0.2
	else 
		LockCurInc=0.2
	End if
	if bMute=False or bGameInPlay=False then PlaySoundVol "sfx_sarc_lock", VolSfx
	tmrBallLock.Interval=20
	tmrBallLock.Enabled=True
End Sub 

Dim lock1Rad:lock1Rad=0
Dim lock2Rad:lock2Rad=0


Sub HandleMummyLock(bRealLock)
	if MummyTimes(CurrentPlayer)=0 then 	' First time you have to spell Mummy
		RotateRamp(1)
		MummyCount(CurrentPlayer)=0
		SSetLightColor kModeMummy, kLightCaptiveBall, red, 2
		SetSlowPulse lCaptiveBall

		if bRealLock then 
			bAutoPlunger=True 
			AddMultiball 1
		End if 
	else 
		SSetLightColor kModeMummy, kLightCaptiveBall, red, 2
		SetFastPulse lCaptiveBall
		MummySarcLock
	End if 
	UpdateMummy2
End Sub 

Sub trgBallLock3_Hit()			' Ball drops in from Ramp
WriteToLog "     ", "trgBallLock3_Hit: "  & MummyTimes(CurrentPlayer)
	BallsInRealLock=BallsInRealLock+1
	if BallsInRealLock=1 then set LockBall1 = ActiveBall
	if BallsInRealLock=2 then set LockBall2 = ActiveBall

	HandleMummyLock True 
End Sub 

Sub trgBallLock2_Hit()		' Knocked ball out of back - Start Mummy MB
WriteToLog "     ", "trgBallLock2_Hit: MummyActive:" & IsModeActive(kModeMummy) & " Times:" & MummyTimes(CurrentPlayer) & " " & BallsInRealLock & " " & BallsInLock(CurrentPlayer)

	' If a real ball is locked then handle it otherwise it means multiplayer and we virtually handle the lock and create a new ball
	if BallsInRealLock>0 then 
		BallsInRealLock=BallsInRealLock-1
		If BallsOnPlayfield-BallsInRealLock>1 then SetMultiballMode True
	Else 
		AddMultiball 1
	End if 

	BallsInLock(CurrentPlayer)=BallsInLock(CurrentPlayer)-1
	RotateRamp(1) 		' Go Back to normal and start MummyMB

	MummyTimes(CurrentPlayer) = MummyTimes(CurrentPlayer) + 1
	if MummyTimes(CurrentPlayer)=1 then
		CycleMummyInserts False
		StartMummyMB
	else										' During MummyMB Knock it out the back to stop timer (and collect 1x JP)
		tmrMummyMBHurryup.Enabled = False
		MummySwitchHitsDoubled(CurrentPlayer)=False
		RemoveHudInfo kModeMummy  	' Go back to single HUD Info without Timer 
		AddHudInfo kModeMummy, "MUMMY", "MULTIBALL", "", "", True
	End if

WriteToLog "     ", "trgBallLock2_Hit Remove Lock:" & BallsInRealLock & " BallsInLock(CurrentPlayer):" & BallsInLock(CurrentPlayer)
End Sub 

Sub trgBallLock0_Hit()		' Ball dropped in tunnel to Underworld 
WriteToLog "     ", "trgBallLock0_Hit: "  & BallsInRealLock & " " & BallsInLock(CurrentPlayer) & " " & GameTime

	if BallsInRealLock>0 then 	' Have to check in case a ball incorrectly fell into the lock
		BallsInRealLock=BallsInRealLock-1
		BallsFromSarcophagus=BallsFromSarcophagus+1
		If BallsOnPlayfield-BallsInRealLock>1 then 
			SetMultiballMode True
		else 
			SetMultiballMode False
		End if 
		BallsInLock(CurrentPlayer)=BallsInLock(CurrentPlayer)-1
		bWaitMummyClear=False

		if BallsInRealLock=0 then 
			RotateRamp(1) 		' Go Back to normal and start MummyMB

			If bGameInPlay then ' Dont Play Scene when Table is disabled amd we are ejecting the ball
				QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""MummyMB.mp4"", ""I:MummyMultiball\\txtMummyMB.png^^^^^^^^^"" ", 1067, 1	
				QueueScene "SceneClearLabels", 0, 1
			End if 

		End if 
	Else 
		' TBD - What do we do here if there is no ball in the lock?
		BallsInLock(CurrentPlayer)=BallsInLock(CurrentPlayer)-1
		if BallsInLock(CurrentPlayer)<0 then BallsInLock(CurrentPlayer)=0	' Jsut in case 
		BallsFromSarcophagus=BallsFromSarcophagus+1
	End if 
End Sub 

Sub trgSarcGate_Hit()		' Backwall ramp to Sarcoghus Lock

WriteToLog "     ", "trgSarcGate_Hit:" & tmrHeadOpen.Enabled & " " & bTombTreasureReady(CurrentPlayer) & " " & MummyLockReady(CurrentPlayer)

	if bTombTreasureReady(CurrentPlayer) Then
		AwardTombTreasure
'		Exit Sub
	End if

	if IsModeActive(kModeMummy) and bMummyDisabled=False and MummyLockReady(CurrentPlayer) then ' GetLightState(kModeMISC, kLightRampLeft)<>0 then 
		if MummyTimes(CurrentPlayer)=0 then 
			QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""MLock.mp4"", ""^^^^^^^^^"" ", 7000, 1
		Else
			Addscore 150000
			MummySwitchHitsDoubled(CurrentPlayer)=True
			QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""ScepterLock.mp4"", ""I:MummyMultiball\\SarcLock.png^^^^^^^^^"" ", 4000, 1
		End if
		QueueScene "SceneClearLabels", 0, 1
	End if 

	Trigger008_Hit()	' Same as travelling the Ramp
	OpenRamp False		' Close the Ramp 

	' Temporarly make ball lock collidable so ball doesnt fall into ball lock when it it is unload mode (Optionally I could add collidable prims in each Position)
	tmrResetCollidable=False
	if gRampPos<2 and primBallLock.Collidable = False Then		' 3 is waiting to load and 2 is waiting to unload (which we handle below)
		primBallLock.Collidable = True
		tmrResetCollidable=True
	End if 

	if tmrHeadOpen.Enabled=False then 		' First one through
WriteToLog "     ", "Open Head gRampPos:" & gRampPos
		if IsModeQual(kModeMummy)=False and gRampPos=3 then
			ModeWaitPlayfieldQual(CurrentPlayer, kModeMummy)=True		' Wait for qual to unpause timer.
			PauseTimersForce 50000		' Pause for a long time (50 Seconds)  
		Else 
			PauseTimersForce 5000
		End if 
		tmrHeadState=0
		tmrHeadOpen.UserValue=0
		tmrHeadOpen.Interval=40
		tmrHeadBallsRemain=0
		bSkipSecond=False
		if gRampPos=2 then bSkipSecond=True 		' Ramp is Waiting to unload a ball, set this to lower the ramp so ball skips the lock

		tmrHeadOpen.Enabled=True
	else 
		tmrHeadBallsRemain=tmrHeadBallsRemain+1		' Corner case when 2 balls slip through
	End if 

End Sub 

Dim HeadRad:HeadRad= SQR((primRotHeadOrigin.Z - PharaohHeadPrim.Z)^2 + (primRotHeadOrigin.Y - PharaohHeadPrim.Y)^2)
Dim tmrHeadState:tmrHeadState=0
Dim tmrResetCollidable
dim tmrHeadBallsRemain
dim bSkipSecond
Sub tmrHeadOpen_Timer()
	Select case tmrHeadState
		case 0:
			tmrHeadOpen.UserValue=tmrHeadOpen.UserValue+1
			if tmrHeadOpen.UserValue >=3000/tmrHeadOpen.Interval then 
				tmrHeadState=tmrHeadState+1
				tmrHeadOpen.UserValue=0

WriteToLog "     ", "tmrHeadOpen_Timer: 2nd ball:" & bSkipSecond & " " & gRampPos

				if bSkipSecond and gRampPos=2 then 				' Ramp will be block the 2nd ball so drop it and rotate it back
					vpmtimer.addtimer 2000, "RotateRamp 1 '"	' Rotate down 
					vpmtimer.addtimer 4000, "RotateRamp 2 '"	' Rotate it back 
				End if 
			End if 
		case 1:
			tmrHeadOpen.UserValue=tmrHeadOpen.UserValue-5
'			tmrHeadOpen.UserValue=tmrHeadOpen.UserValue-2.5
			bRotXInv=False 
'			DoRotateO primRotHeadOrigin, PharaohHeadPrim, HeadRad, tmrHeadOpen.UserValue, 20
			PharaohHeadPrim.rotx=tmrHeadOpen.UserValue
			PharaohHeadBracketPrim.rotX = tmrHeadOpen.UserValue
			If tmrHeadOpen.UserValue <=-30 then 
WriteToLog "     ", "tmrHeadOpen_Timer:" & BallsInRealLock & " " & gRampPos & " " & IsModeActive(kModeMummy) & " " & bSkipSecond & " " & MummyLockReady(CurrentPlayer)
				tmrHeadState=tmrHeadState+1

				If BallsInRealLock = 0 and gRampPos=3 then							' Lets it fall into lock (Tricky because other things can fall in if we leave it open too Long)
					primBallLock.Collidable = False								
					VpmTimer.AddTimer 1500, "primBallLock.Collidable = True '"
				End if 
				Wall057.Collidable=False

				if bSkipSecond=False and IsModeActive(kModeMummy) and MummyLockReady(CurrentPlayer) then 			' Make sure the mode is active to lock a ball (instead of just TombTreasure)
WriteToLog "     ", "Adding Locked Ball:"
					BallsInLock(CurrentPlayer)=BallsInLock(CurrentPlayer)+1		' Add Player Locked ball
					if BallsInRealLock>0 then HandleMummyLock False 
'					SSetLightColor kModeMISC, kLightRampLeft, red, 0
					MummyLockReady(CurrentPlayer)=False
				End if 
			End if 
		case 2: 
			tmrHeadOpen.UserValue=tmrHeadOpen.UserValue+5
			bRotXInv=False 
'			DoRotateO primRotHeadOrigin, PharaohHeadPrim, HeadRad, tmrHeadOpen.UserValue, 20
			If tmrHeadOpen.UserValue >=0 then 
				tmrHeadOpen.UserValue=0
				tmrHeadState=tmrHeadState+1
				Wall057.Collidable=True
			End if 
			PharaohHeadPrim.rotx=tmrHeadOpen.UserValue
			PharaohHeadBracketPrim.rotX = tmrHeadOpen.UserValue
		case 3:
			PauseTimersForce 2000		' Reset for 2 more seconds 
			if tmrResetCollidable then VpmTimer.AddTimer 1500, "primBallLock.Collidable=False '"

			' There are multiple balls behind the PharoahHead
			if tmrHeadBallsRemain>0 then 
WriteToLog "     ", "Handle 2nd ball:" & gRampPos
				bSkipSecond=True 							' Ignore this as a lock since it is erroneous
				tmrHeadBallsRemain=tmrHeadBallsRemain-1
				tmrHeadState=0
				tmrHeadOpen.UserValue=0
				tmrHeadOpen.Interval=40
				tmrHeadOpen.Enabled=False 
				tmrHeadOpen.Enabled=True

				' Ramp will be block the 2nd ball so drop it and rotate it back
				PauseTimersForce 4000 
			else 
WriteToLog "     ", "Head done:"
				bSkipSecond=False
				tmrHeadOpen.Enabled = False 
			End if 
	End Select 

End Sub 

Sub EnableMummy(bEnabled)
WriteToLog "     ", "EnableMummy:" & bEnabled & " " & bMummyDisabled & " " & bGuardianOpen(CurrentPlayer) & " " & FlashGuardianState
	if bMummyDisabled = bEnabled then 
		if bEnabled=False then 
			bMummyDisabled=True
			bGuardianOpen(CurrentPlayer)=(FlashGuardianState=2) 	' Save State 
			OpenRamp2 False, True 
'			SSetLightColor kModeMISC, kLightRampLeft, red, 0
		Else
			bMummyDisabled=False
			OpenRamp2 bGuardianOpen(CurrentPlayer), True 
'			if bGuardianOpen(CurrentPlayer) then SSetLightColor kModeMISC, kLightRampLeft, red, 2
		End if 
	End if 
End Sub 

Sub OpenRamp(bOpen)						' Open Guardian Left Ramp
	OpenRamp2 bOpen, False
End Sub 

Sub OpenRamp2(bOpen, bForce)						' Open Guardian Left Ramp
WriteToLog "     ", "OpenRamp2:" & bOpen & " Force:" & bForce & " GuardLightState:" & FlashGuardianState
	tmrOpenGuardian.Interval = 50
	if bOpen then 
		if FlashGuardianState<>2 or bForce then ' Not already open 
			Flashforms l165_spot, 4000, 20, 0
			FlashGuardianState=2
			tmrOpenGuardian.UserValue = 1
			tmrOpenGuardian.enabled = True 
			SSetLightColor kModeMISC, kLightRampLeft, red, 2
		End if 
	Else 
		if FlashGuardianState<>0 or bForce then ' Not already closed 
			FlashGuardianState=0
			tmrOpenGuardian.UserValue = -1
			tmrOpenGuardian.enabled = True 
			SSetLightColor kModeMISC, kLightRampLeft, red, 0		' Make sure this int flashing
		End if 
	End if 
End Sub

Sub tmrOpenGuardian_Timer()
	If MotorCheck=True Then 'Using Check only fire motor when game in play
		DOF 221, Dofpulse 'gear motor	
	End if 
	GuardianPrim.z = GuardianPrim.z+tmrOpenGuardian.UserValue
	wallRampOpenPrim.z=wallRampOpenPrim.z+tmrOpenGuardian.UserValue
	if tmrOpenGuardian.UserValue>0 then 	' Opening 
		if GuardianPrim.z>38 then
			wallRampOpenPrim.Collidable=False 
			tmrOpenGuardian.enabled=false 
		End if 
	else ' Closing 
		if GuardianPrim.z<=0 then
			wallRampOpenPrim.Collidable=True 
			tmrOpenGuardian.enabled=False
		End if 
	end if 
End Sub 

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

	if bDebugMode=False then 
'		If keycode = LeftMagnaSave Then bLutActive = True
'		If keycode = RightMagnaSave Then
'			If bLutActive Then myNextLUT:End If
'		End If
	End if 

WriteToLog "     ", "KEYDOWN: " & keycode
	if keycode = 19 then 	' r key (debugging )
'		BallSaverTimerCancel
'		ProcessHallowed kLightRampCenter

	End if 

	if keycode = 18 then 	' e key (debugging )
'		GILightRed True
'		GILightWhite False

'		FlashFlasherSetSpeed 1, 0
'		BallsOnPlayfield=BallsOnPlayfield+1

'		HallowedCount=5
'		ProcessHallowed kLightRampLeft


	End if 

	if keycode = 17 then 	' w key (debugging )

'		Wall005_guide.collidable=True
'		bDropSweep=True
'		DropCount=3
'		CheckDrops 1
'		CheckPowerTargets

'		FlashFlasherSetSpeed 1, 2
'		bFlasher1Enabled=True

'		GILightRed False
'		GILightWhite True 

' Center Ramp
'		Kicker001.CreateSizedBallWithMass BallSize / 2, BallMass
'		Kicker001.Kick 0, 70

'		Mystery_AddaBall

'		NewtonHit 2
'		PlaySoundVol "sfx_bullseye",VolSfx
		'QueueScene "PlaySoundVol ""sfx_MummySpell2"", VolSfx ", 1, 1

'		Flashforms l165_spot, 4000, 20, 0
'		Flashforms l166a_spot, 4000, 20, 0
'		Flashforms l166b_spot, 4000, 20, 0
''		Flashforms l165_spot, 3000, 300, 0			' Flash the spotlight 
'		FlashFlasher1(1)
'		FlashFlasher1(2)
'		FlashFlasher1(3)
'		FlashFlasher1(4)
'		FlashFlasher1(5)

'		tmrHeadBallsRemain=tmrHeadBallsRemain+1
'		Activeball.x=355.3816
'		Activeball.y=349.9201
'		Activeball.z=23.87199

'		SceneGeneralStart pDMDFull, False, False, "FearOfTheDark", "FearOfDarkLoop.mp4", "8:BONUS^^^^^^^^^", "^^^^^^^^^"

'		DoAnimatePF 1
		RotateMode
'		if RampLock0.Collidable = True then 		' gRampPos=0
'			RotateRamp(3)
'		elseif RampLock3.Collidable = True then 	' gRampPos=3
'			RotateRamp(2)
'		elseif RampLock2.Collidable = True then 	' gRampPos=2
'			RotateRamp(1)
'		Else 										' gRampPos=1
'			RotateRamp(0)
'		End if 

' RTTH
'		TombTreasureCount(CurrentPlayer)=9
'		StartTombTreasure(kTombShardRime)

' NOTB
'		StopEDDIELetter
'		SetModeActive kModeEddie, False
'		SetModeActive kModeNOTB, True
'		ResetEDDIELetter
'		SetupEddieInserts

' Madness
'		TombTreasureCount(CurrentPlayer)=2
'		StartTombTreasure(kTombShardRime)

	End if 
	if keycode = 18 then 	' e key (debugging )

'		EndNOTB True 
'		Mode(CurrentPlayer, kModeIcarus)=1
'		Mode(CurrentPlayer, kModeFear)=1
'		Mode(CurrentPlayer, kModeAces)=1
'		Mode(CurrentPlayer, kModeHallowed)=1
'		Mode(CurrentPlayer, kModeRime)=1
	End if 
	if keycode = 19 then 	' r key (debugging )
'		CapKicker2a.kick -8, 45
		CapKicker2.kick 0, 1
		CapKicker2.enabled = True 
		VpmTimer.AddTimer 300, "CapKicker2.enabled = False '"
		'DoRotateO primBallLockOrigin, LockBall1, lock1Rad, LockCurPos, 0
	End if 

	StartServiceMenu keycode

    If Keycode = AddCreditKey and Credits<10 Then
'        Credits = Credits + 1
		Dof 220,Dofpulse 'strobes for credit
		AddCredit 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
			TimerStats(False) 
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin" 'Could add pupevent here CP
			pDMDShowLines2 "CREDITS " & credits ,"PRESS START BUTTON",2
            'If NOT bGameInPlay Then ShowTableInfo
			PlaySoundVol "sfx_Credit2", VolSfx
			if pInAttract then 
				pCurAttractPos=pAttractCredit-1
				pAttractNext
			End if 
        End If
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        SoundPlungerPull()
		If VRRoom > 0 then 
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

	' nFozzy
	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then FlipperActivate LeftFlipper1, LFPress1
		If VRRoom > 0 Then FlipperButtonLeft.x = FlipperButtonLeft.x + 5
	End if 
	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperActivate RightFlipper1, RFPress1
		If VRRoom > 0 Then FlipperButtonRight.x = FlipperButtonRight.x - 5
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then FlipperActivate RightFlipper1, RFPress1: ProcessMadness(RightFlipperKey)
		If keycode = KeyUpperLeft Then FlipperActivate LeftFlipper1, LFPress1: ProcessMadness(LeftFlipperKey)
	End If

    If keycode = StartGameKey Then
		If VRRoom > 0 Then 
			PinCab_Button1i.y = PinCab_Button1i.y - 1
			PinCab_Button1o.y = PinCab_Button1o.y - 1
		End If
	End If	


    If hsbModeActive Then
		if (keycode = LeftFlipperKey or keycode = RightFlipperKey) then 		' Press and hold on HighScoreEntry
			tmrHSHoldFlipper.Interval = 300
			tmrHSHoldFlipper.Enabled = True 
		End if 

        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

' ******************* GENERAL GAMEPLAY ************************
    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft():CheckTilt False 
        If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight():CheckTilt False 
        If keycode = CenterTiltKey Then Nudge 0, 2:SoundNudgeCenter():CheckTilt False 
		If keycode = MechanicalTilt then CheckTilt True


        If keycode = LeftFlipperKey and bTableDisabled=False Then 
			SolLFlipper 1
			If StagedFlipperMod <> 1 Then SolULFlipper 1
			StartInstantInfo(keycode)
			RotateActivateX
			UpdateSkillShot
			If StagedFlipperMod = 0 then ProcessMadness(keycode)
		End If
        If keycode = RightFlipperKey and bTableDisabled=False Then
			SolRFlipper 1
			If StagedFlipperMod <> 1 Then SolURFlipper 1
			StartInstantInfo(keycode)
			RotateActivateX
			If StagedFlipperMod = 0 then ProcessMadness(keycode)
		End If
		If StagedFlipperMod =1 and keycode = KeyUpperRight then SolURFlipper 1
		If StagedFlipperMod =1 and keycode = KeyUpperLeft then SolULFlipper 1

		' Debug mode 
		If (keycode = LeftMagnaSave or keycode = RightMagnaSave) then 
			HandleDebugDown(keycode)
		End If

		' ***************************************
		' FlippserSkipCallbacks
		BonusSceneCallback
		NOTBCallback
		' ***************************************


        If keycode = StartGameKey Then 
			soundStartButton()

			if bBallInPlungerLane and LFPress and keycode = StartGameKey and DMDStd(kDMDStd_LeftStartReset) then 		' Reset when holding start and left flipper  
				tmrHoldKey.Enabled = False
				tmrHoldKey.Enabled = True
			End If

			If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then
                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, "" 
					PlaySoundVol "vo_player" & PlayersPlayingGame, VolDef
					PupOverlayBonusSc 'ANDREW
					UpdatePlayers()
				Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
'                        Credits = Credits - 1
						AddCredit -1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
						PlaySoundVol "vo_player" & PlayersPlayingGame, VolDef
						PupOverlayBonusSc 'ANDREW
						UpdatePlayers()
						If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
					Else
						' Not Enough Credits to start a game.
						PlaySoundVol "sfx_Credit1", VolSfx
						DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""  'Easter egg pupevent CP CHRIS
						pDMDShowLines2 "CREDITS " & credits ,"INSERT COIN",2
						if pInAttract then 
							pCurAttractPos=pAttractCredit-1
							pAttractNext
						End if
                    End If
                End If
				
            End If
			
	    End If
    ElseIf bShowMatch = False then ' If (GameInPlay)

		If keycode = StartGameKey Then 
			soundStartButton()
			If(bFreePlay = True)Then
				PlaySoundVol "vo_freeplay", VolDef
				If(BallsOnPlayfield = 0)Then
					ResetForNewGame()
				End If
			Else
				If(Credits> 0)Then
					If(BallsOnPlayfield = 0)Then
						'Credits = Credits - 1 : 
						AddCredit -1
						If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
						ResetForNewGame()
					End If
				Else
					' Not Enough Credits to start a game
					PlaySoundVol "sfx_Credit1", VolSfx
					DMDFlush
					TimerStats(False) 
					DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""  'Easter egg pupevent CP CHRIS
					pDMDShowLines2 "CREDITS " & credits ,"INSERT COIN",2
					'ShowTableInfo
					if pInAttract then 
						pCurAttractPos=pAttractCredit-1
						pAttractNext
					End if 
				End If
			End If
		End If
		

		' Cycle attract 
		if BallsOnPlayfield = 0 and bInService=False then 				' Dont allow this if balls are still on the PF
			if LFPress and RFPress then 
				pCurAttractPos=pAttractLastScore-1
				pAttractNext
			else 
				If keycode = LeftFlipperKey Then
					pCurAttractPos=pCurAttractPos-2
WriteToLog "     ", "LEFT pCurAttractPos=" & pCurAttractPos & " TimerStats_Enabled:" & TimerStats_Enabled
					if pCurAttractPos=-1 then pCurAttractPos=pAttractEndOfLoop-1
					if pCurAttractPos>pAttractEndOfLoop-1 then pCurAttractPos=pAttractEndOfLoop-2
					if pCurAttractPos=pAttractLastScore-1 then 
						if LastScore(0)=0 then 
							pCurAttractPos=0
						End if 
					End if 
					if (TimerStats_Enabled and tmrStats_idx<>0) or pCurAttractPos=pAttractScrollEnd-1 then 
						pCurAttractPos=pAttractScrollStart-1
					End if 
					pAttractNext
				End if 
				If keycode = RightFlipperKey Then
					tmrStats_increment=0.75*4
					pAttractNext
				End If
				if keycode = RightFlipperKey or keycode = LeftFlipperKey then 	' Do callout 
					if AttractTimer.UserValue=0 then 
						AttractTimer_Timer
						AttractTimer.UserValue=1
					End if 
				End if 
			End if 
		End if 
	elseIf bShowMatch and keycode = StartGameKey Then   'Skip Show Match 
		StopSound "Sfx_MatchB"
		EndOfGame()
	End If ' If (GameInPlay)
End Sub

Sub PupOverlayInGame
WriteToLog "     ", "PupOverlayInGame"
	UpdatePowerFeature
	pPowerFeatures
	PupOverlayHUD(False)	
	QueueSetDefault 0, "UpdateEDDIELetter", ""
End Sub

'Sub PupOverlaySongSelect
'	PupOverlayHUD(True)	
'	QueueSetDefault 0, "UpdateEDDIELetter", ""
'End Sub

Sub PupOverlayBonusSc
	PupOverlayHUD(False)
End Sub

Sub PupOverlayMatch
	PupOverlayHUD(True)
End Sub

Sub PupOverlayScore
	PupOverlayHUD(True)
End Sub 


Sub AddHudInfo2(mode, txt1, txt2)
	tmrHUDAnimate2_Mode=mode
	tmrHUDAnimate2_InitialValue1=txt1
	tmrHUDAnimate2_InitialValue2=txt2
	tmrHUDAnimate2_Enabled=True
	tmrHUDAnimate2_Height=0
	tmrHUDAnimate2_HeightEnd=20
End Sub

Sub RemoveHudInfo2(Mode)
	if tmrHUDAnimate2_Mode=Mode then 
		tmrHUDAnimate2_Mode=-1 
		tmrHUDAnimate2_Enabled=False
		tmrHUDAnimate2_Height=0
		tmrHUDAnimate2_HeightEnd=0
		tmrHUDAnimate_Timer
		puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\clear.png",1,""

		PuPlayer.LabelSet pDMDFull,"ModeTxt2HL1"," ",1,""
		PuPlayer.LabelSet pDMDFull,"ModeTxt2HL2"," ",1,""
	End if 
End Sub

Sub RemoveHudInfo(Mode)
	Dim SaveMode
	Dim SaveTxt1
	Dim SaveTxt2
	Dim bRestoreMode:bRestoreMode=False 
WriteToLog "     ", "RemoveHudInfo:" & Mode & " 1:" & tmrHUDAnimate_Mode & " 2:" & tmrHUDAnimate2_Mode

	If tmrHUDAnimate2_Mode<>-1 and tmrHUDAnimate2_Mode<>Mode and tmrHUDAnimate_Mode=Mode then ' Bottom HUD will Close but top will still open, Move it down 
		SaveMode=tmrHUDAnimate2_Mode
		SaveTxt1=tmrHUDAnimate2_InitialValue1
		SaveTxt2=tmrHUDAnimate2_InitialValue2
		RemoveHudInfo2 SaveMode	
		bRestoreMode=True 
	End if 

	RemoveHudInfo2 Mode	
	if tmrHUDAnimate_Mode=Mode then 
		tmrHUDAnimate_Mode=-1 
		tmrHUDAnimate_Enabled=False
		tmrHUDAnimate_Height=0
		tmrHUDAnimate_HeightEnd=0
		tmrHUDAnimate_Timer
		puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\clear.png",1,""

		PuPlayer.LabelSet pDMDFull,"ModeTxtL1"," ",1,""
		PuPlayer.LabelSet pDMDFull,"ModeTxtL2"," ",1,""
		PuPlayer.LabelSet pDMDFull,"ModeTxtL3"," ",1,""
		PuPlayer.LabelSet pDMDFull,"ModeTimer"," ",1,""		
		PuPlayer.LabelSet pDMDFull,"ModeTxtHL1"," ",1,""
		PuPlayer.LabelSet pDMDFull,"ModeTxtHL2"," ",1,""
	End if 

	If bRestoreMode then AddHudInfo SaveMode, SaveTxt1, SaveTxt2, "", "", True 

	WriteToLog "     ", "         End:" & Mode & " 1:" & tmrHUDAnimate_Mode & " 2:" & tmrHUDAnimate2_Mode

End Sub

Sub AddHudInfo(mode, txt1, txt2, txt3, txtTimer, bHalfHeight)
	if tmrHUDAnimate_Mode<>-1 and bHalfHeight then		' Primary Mode is already full Use Secondary Mode
WriteToLog "     ", "AddHudInfo2"
		AddHudInfo2 mode, txt1, txt2
	Else 
WriteToLog "     ", "AddHudInfo"
		tmrHUDAnimate_Mode=mode
		tmrHUDAnimate_InitialValue1=txt1
		tmrHUDAnimate_InitialValue2=txt2
		tmrHUDAnimate_InitialValue3=txt3
		tmrHUDAnimate_InitialValue4=txtTimer
		tmrHUDAnimate_Height=45
		if bHalfHeight then 
			tmrHUDAnimate_HeightEnd=28
		Else
			tmrHUDAnimate_HeightEnd=18
		End if 
		tmrHUDAnimate_Enabled=True 
	End if 
	tmrHUDAnimate.Interval = 30
	tmrHUDAnimate.Enabled = True 
End Sub

'puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':52, 'xalign':2}"
'puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':82, 'xalign':2}"
'puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':62, 'xalign':2}"

Dim tmrHUDAnimate2_Mode:tmrHUDAnimate2_Mode=-1
Dim tmrHUDAnimate2_Height
Dim tmrHUDAnimate2_HeightEnd:tmrHUDAnimate2_HeightEnd=0
Dim tmrHUDAnimate2_Enabled:tmrHUDAnimate2_Enabled=False
Dim tmrHUDAnimate2_InitialValue1
Dim tmrHUDAnimate2_InitialValue2
Dim tmrHUDAnimate_Enabled:tmrHUDAnimate_Enabled=False
Dim tmrHUDAnimate_Mode:tmrHUDAnimate_Mode=-1 
Dim tmrHUDAnimate_Height
Dim tmrHUDAnimate_HeightEnd
Dim tmrHUDAnimate_InitialValue1
Dim tmrHUDAnimate_InitialValue2
Dim tmrHUDAnimate_InitialValue3
Dim tmrHUDAnimate_InitialValue4
Sub tmrHUDAnimate_Timer
	dim bDone:bDone=True 
	Dim firstEnd:firstEnd=tmrHUDAnimate_HeightEnd

	if tmrHUDAnimate_Enabled then 
		tmrHUDAnimate_Height=tmrHUDAnimate_Height-4
		if tmrHUDAnimate_Height<tmrHUDAnimate_HeightEnd then 
			puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\HUD-3.png",1,"{'mt':2,'width':16, 'height':58, 'xpos':100 ,'ypos':"&tmrHUDAnimate_HeightEnd&", 'xalign':2}"
			if tmrHUDAnimate_HeightEnd=18 then 
				PuPlayer.LabelSet pDMDFull,"ModeTxtL1",tmrHUDAnimate_InitialValue1,1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtL2",tmrHUDAnimate_InitialValue2,1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtL3",tmrHUDAnimate_InitialValue3,1,""
				PuPlayer.LabelSet pDMDFull,"ModeTimer",tmrHUDAnimate_InitialValue4,1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtHL1"," ",1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtHL2"," ",1,""
			else 
				PuPlayer.LabelSet pDMDFull,"ModeTxtL1"," ",1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtL2"," ",1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtL3"," ",1,""
				PuPlayer.LabelSet pDMDFull,"ModeTimer"," ",1,""		
				PuPlayer.LabelSet pDMDFull,"ModeTxtHL1",tmrHUDAnimate_InitialValue1,1,""
				PuPlayer.LabelSet pDMDFull,"ModeTxtHL2",tmrHUDAnimate_InitialValue2,1,""
			End if 
			tmrHUDAnimate_Enabled=False 
		else 
			firstEnd=tmrHUDAnimate_Height
			bDone=False 
			puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\HUD-3.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':"&tmrHUDAnimate_Height&", 'xalign':2}"
		End if
	End if

	if tmrHUDAnimate2_Enabled then 
		tmrHUDAnimate2_Height=tmrHUDAnimate2_Height+4
		if tmrHUDAnimate2_HeightEnd>=tmrHUDAnimate2_HeightEnd then 
			puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':"&firstEnd-tmrHUDAnimate2_HeightEnd&", 'xalign':2}"			
			PuPlayer.LabelSet pDMDFull,"ModeTxt2HL1",tmrHUDAnimate2_InitialValue1,1,""
			PuPlayer.LabelSet pDMDFull,"ModeTxt2HL2",tmrHUDAnimate2_InitialValue2,1,""
			tmrHUDAnimate2_Enabled=False 
		else 
			bDone=False 
			puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':"&firstEnd-tmrHUDAnimate2_Height&", 'xalign':2}"
		End if
	elseif tmrHUDAnimate_Enabled and tmrHUDAnimate2_Mode<>-1 then 
		puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\HUD-4.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':"&firstEnd-tmrHUDAnimate2_HeightEnd+1&", 'xalign':2}"
	End if 

	if bDone then 
		tmrHUDAnimate.Enabled = False
	End if 
End Sub 

Sub PupOverlayHUD(bBaseOnly)
	WriteToLog "     ", "PupOverlayHUD:" & bBaseOnly

	if bShowMatch then 
		PuPlayer.playlistplayex pDMdFull, "PuPOverlays" ,"HUD-BG2.png",  1, 1
	Else 
		PuPlayer.playlistplayex pDMdFull, "PuPOverlays" ,"HUD-BG.png",  1, 1
	End if 

	if bBaseOnly then 
		puPlayer.LabelSet pDMdFull, "HUD1", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "HUD2", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\clear.png",1,""
	else
		puPlayer.LabelSet pDMdFull, "HUD1", "PupOverlays\\HUD-1.png",1,"{'mt':2,'width':16, 'height':31, 'xpos':100 ,'ypos':100, 'xalign':2}"
		puPlayer.LabelSet pDMdFull, "HUD2", "PupOverlays\\HUD-2.png",1,"{'mt':2,'width':16, 'height':45, 'xpos':100 ,'ypos':90, 'xalign':2}"
	End if


	Select Case PlayersPlayingGame
		case 1:
			puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\clear.png",1,""
			puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\clear.png",1,""
			puPlayer.LabelSet pDMdFull, "P3Bg", "PupOverlays\\clear.png",1,""
			puPlayer.LabelSet pDMdFull, "P4Bg", "PupOverlays\\clear.png",1,""
		case 2:
			puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':51 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':76 ,'ypos':100, 'xalign':2}"
		case 3:
			puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':39.5 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':64 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P3Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':89 ,'ypos':100, 'xalign':2}"
		case 4:		
			puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':26 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':51 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P3Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':75 ,'ypos':100, 'xalign':2}"
			puPlayer.LabelSet pDMdFull, "P4Bg", "PupOverlays\\HUD-Players.png",1,"{'mt':2,'width':28, 'height':8.5, 'xpos':100 ,'ypos':100, 'xalign':2}"
	End Select 
End Sub


Sub UpdatePlayers()
	Select case PlayersPlayingGame
		case 1:
		case 2:
			PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':38}"
			PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':62}"
		case 3:
			PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':25}"
			PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':50}"
			PuPlayer.LabelSet pDMDFull,"ScorePos3",FormatScore(Score(2)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':75}"
		case 4:
			PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':13}"
			PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':38}"
			PuPlayer.LabelSet pDMDFull,"ScorePos3",FormatScore(Score(2)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':63}"
			PuPlayer.LabelSet pDMDFull,"ScorePos4",FormatScore(Score(3)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':87}"
	End Select
End Sub 

Sub Table1_KeyUp(ByVal keycode)
	if bTableDisabled then exit sub
    
	tmrHoldKey.Enabled = False
    If keycode = PlungerKey Then
        Plunger.Fire
        SoundPlungerReleaseBall()
		If VRRoom > 0 then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End if
	End If

    If keycode = StartGameKey Then
		If VRRoom > 0 Then 
			PinCab_Button1i.y = -96.97916
			PinCab_Button1o.y = -95.51598
		End If
	End If	

    If bSongSelect Then
        SelectSong(keycode)
    End If
			
    If hsbModeActive Then
		if keycode = LeftFlipperKey or keycode = RightFlipperKey then 
			tmrHSHoldFlipper.Enabled = False 
		End if
		If keycode = LeftFlipperKey Then lfpress = 0
		If keycode = RightFlipperKey Then rfpress = 0

		StopInstantInfo()
        Exit Sub
    End If

	HandleDebugUp(keycode)

	If keycode = LeftMagnaSave Then bLutActive = False
	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		If VRRoom > 0 Then FlipperButtonLeft.x = 2109
	End If
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperDeActivate RightFlipper1, RFPress1
		If VRRoom > 0 Then FlipperButtonRight.x = 2087
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then FlipperDeActivate RightFlipper1, RFPress1
	End If


    ' Table specific
    If bGameInPLay AND NOT Tilted Then

        If keycode = LeftFlipperKey Then
            SolLFlipper 0
			UpdateSkillShot
            If StagedFlipperMod <>1 then SolULFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            If StagedFlipperMod <>1 then SolURFlipper 0
        End If
		If StagedFlipperMod =1 and keycode = KeyUpperRight then SolURFlipper 0
		If StagedFlipperMod =1 and keycode = KeyUpperLeft then SolULFlipper 0


		If keycode = LeftFlipperKey or keycode = RightFlipperKey Then
			EndFlipperStatus(keycode)
		End If 
    End If
End Sub

Sub tmrHoldKey_Timer()		' Reset the game with Ball in lane
	tmrHoldKey.Enabled = False

WriteToLog "     ", "tmrHoldKey_Timer:"  & bBallInPlungerLane  & " " & BallsOnPlayfield-BallsInRealLock
	If bBallInPlungerLane and BallsOnPlayfield-BallsInRealLock = 1 Then
		PlaySoundVol "start", VolDef
		bResetCurrentGame=True
		DisableTable True
		AutoPlungeDelayed
		VpmTimer.Addtimer 1000, "EndOfGame '"						' End game to drain locked balls, but need to wait until ball launches 
	End If
End Sub

sub tmrHSHoldFlipper_Timer()
WriteToLog "     ", "AAA " & RFPress & " " & LFPress
	tmrHSHoldFlipper.Interval = 100			' Speed it up
	if RFPress then 
		EnterHighScoreKey(RightFlipperKey)
	elseif LFPress then 
		EnterHighScoreKey(LeftFlipperKey)
	End if 
End Sub 


'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
	Scorbit.StopSession2 Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame, True 	' In case you stop mid game 
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = True Then Controller.Stop
End Sub

'********************
'     Flippers
'********************
Const ReflipAngle = 20
Const QuickFlipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
		DOF 101, DOFOn
		LF.Fire    'LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
'        LeftFlipper1.RotateToEnd
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If	
    Else
		DOF 101, DOFOff
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        DOF 102, DOFOn
        RF.Fire 'RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
 '       RightFlipper1.RotateToEnd
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
    Else
		DOF 102, DOFOff
        RightFlipper.RotateToStart
'        RightFlipper1.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		RightFlipper1.RotateToEnd
		If RightFlipper1.currentangle > RightFlipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else 
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
	Else
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If
		If RightFlipper1.currentangle < RightFlipper1.startAngle + QuickFlipAngle and RightFlipper1.currentangle <> RightFlipper1.endangle Then
			'Play quick flip sound and stop any flip up sound
			'StopAnyFlipperUpperRightUp()
			'RandomSoundUpperRightQuickFlipUp()
		Else
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub


Sub SolULFlipper(Enabled)
	If Enabled Then
		LeftFlipper1.RotateToEnd
		If LeftFlipper1.currentangle > LeftFlipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper1
		Else 
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If
	Else
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle > LeftFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle + QuickFlipAngle and LeftFlipper1.currentangle <> LeftFlipper1.endangle Then
			'Play quick flip sound and stop any flip up sound
			'StopAnyFlipperUpperRightUp()
			'RandomSoundUpperRightQuickFlipUp()
		Else
			FlipperLeftHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub


' flippers hit sounds and physics

Sub LeftFlipper_Collide(parm)
	ResetBallSearch()
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	ResetBallSearch()
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub


'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts 1 from the "Tilt" variable every round
Dim TiltDangerWait
TiltDangerWait=False
Sub CheckTilt(bReal)                           'Called when table is nudged
	if HasRealTiltBob and bReal=False then exit sub		' Real tilt bob does the tilt 
    If NOT bGameInPlay Then Exit Sub
	If BallsOnPlayfield-BallsInRealLock=0 then exit sub 	' Cant tilt when no balls are on the table 
	If TiltDangerWait then Exit Sub				' Debounce wait

    Tilt = Tilt + TiltSensitivity              'Add to tilt count

	TiltDecreaseTimer.Interval = 200
    TiltDecreaseTimer.Enabled = True
	if (bReal or Tilt > 15) and TiltCount(CurrentPlayer) < DMDStd(kDMDStd_TiltWarn) Then 				' Tilt Warning 
		TiltDangerWait=True 
		TiltCount(CurrentPlayer) = TiltCount(CurrentPlayer) + 1

        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 1000, True, ""
		pupevent 607
		PlaySoundVol "vo_danger" & INT(RND*5)+1, VolDef
'		DOF 311, DOFPulse	'Tilt Warning

		GIOff
		'vpmtimer.addtimer 1000, "GIOn:PlaySoundVol ""sfx-Tilt1"", VolSfx '"		' Jack has these as 2 sounds and animations (Special setting??) 
		vpmtimer.addtimer 1000, "GIOn '"
		vpmtimer.addtimer DMDStd(kDMDStd_TiltDebounce), "SceneClearTilt '"			' Disable any tilts for a while
	
	ElseIf bReal or Tilt > 15 Then 											'If more that 15 then TILT the table
        Tilted = True
		DOF 167, DOFPulse 'Tilt Effect
		PlaySoundVol "vo_tilt", VolSfx
'		playclear pOverVid
'		PuPlayer.LabelShowPage pOverVid, 1,0,""
'		playmedia "Tilt.mp4", "Callouts", pOverVid, "", -1, "", 1, 1
'		DOF 310, DOFPulse	'TILT

        DMDFlush
        DMD "", "", "DMD_tilt", eNone, eNone, eBlink, 2000, True, ""
		pupevent 606

		GIOff
		vpmtimer.addtimer 4000, "SceneClearTilt '"
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
	End if 

End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
WriteToLog "     ", "TiltDecreaseTimer_Timer: " & Tilt
    If Tilt>0 Then
        Tilt = Tilt - 1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub SceneClearTilt()
WriteToLog "     ", "SceneClearTilt"

	GIOn
	TiltDangerWait=False
	playclear pOverVid
	if bTableDisabled=False then PupOverlayInGame	' If we tilted we will be on music select dont go back to in Game
End Sub

Sub DisableTable(Enabled)
WriteToLog "     ", "DisableTable:" & Enabled
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
		LeftFlipper1.RotateToStart
        RightFlipper1.RotateToStart

        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
		bTableDisabled=True
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
		bTableDisabled=False
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield-BallsInRealLock = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
' in VPX 10.7 you may use also mp3 or ogg
'*****************************************

Dim Song, Songnr
Song = ""
Songnr = INT(RND * kMaxSongs)

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
		End If
    End If
End Sub

Sub SelectSong(keycode)
	dim bAnimate:bAnimate=False

	if bInstantInfo then exit sub 	' Dont allow song selection during instant info

    If keycode = LeftFlipperKey Then
        Songnr = (Songnr - 1)
        If Songnr <0 Then Songnr = kMaxSongs
		bAnimate=True
		tmrSongSelectAni_Dir=-1
    End If
    If keycode = RightFlipperKey Then
        Songnr = (Songnr + 1)MOD (kMaxSongs+1)
		tmrSongSelectAni_Dir=1
		bAnimate=True
    End If

	if bAnimate then 
		puPlayer.LabelSet pDMdFull, "SongSelectC", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':28, 'height':48, 'xpos':19 ,'ypos':15}"
		tmrSongSelectAni_Type=0
		tmrSongSelectAni.UserValue = 0
		tmrSongSelectAni.Interval=50
		tmrSongSelectAni.Enabled = True 
	End if 
	SelectMusic(Songnr)		' Go ahead and start music now

End Sub

Dim tmrSongSelectAni_Type
Dim tmrSongSelectAni_Dir
Sub tmrSongSelectAni_Timer
	Dim xval1
	Dim xval2
	Dim height
	Dim width
	Dim yval
	Dim SongC:SongC=Songnr-tmrSongSelectAni_Dir
	Dim SongL:SongL=SongC-1
	Dim SongR:SongR=SongC+1

	if SongL<0 then SongL=kMaxSongs
	if SongR>kMaxSongs then SongR=0

	tmrSongSelectAni.UserValue=tmrSongSelectAni.UserValue+1
	if tmrSongSelectAni_Type=0 then 			' Selection animation
		xval1=tmrSongSelectAni.UserValue*2
		xval2=tmrSongSelectAni.UserValue*3
		yval=tmrSongSelectAni.UserValue*1.2
		height=tmrSongSelectAni.UserValue*1.5
		width=tmrSongSelectAni.UserValue
		
		if 4 + xval1 < 19 then 
			if tmrSongSelectAni_Dir<0 then 
				puPlayer.LabelSet pTransp, "SongSelectLT", "SongSelection\\s" & SongL  &".png",1,"{'mt':2,'color':0,'width':" & 18+width & ", 'height':" & 35+height & ", 'xpos':" & 4 + xval1 & " ,'ypos':" & 25 - yval & "}"
				puPlayer.LabelSet pTransp, "SongSelectCT", "SongSelection\\s" & SongC  &".png",1,"{'mt':2,'color':0,'width':" & 28-width & ", 'height':" & 48-height & ", 'xpos':" & 19 + xval2 & " ,'ypos':" & 15 + yval & "}"
				puPlayer.LabelSet pTransp, "SongSelectRT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':18, 'height':31, 'xpos':45 ,'ypos':25}"
			Else 
				puPlayer.LabelSet pTransp, "SongSelectLT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':" & 18+width & ", 'height':31, 'xpos':4 ,'ypos':25}"
				puPlayer.LabelSet pTransp, "SongSelectCT", "SongSelection\\s" & SongC  &".png",1,"{'mt':2,'color':0,'width':" & 28-width & ", 'height':" & 48-height & ", 'xpos':" & 19 - xval1 & " ,'ypos':" & 15 + yval & "}"
				puPlayer.LabelSet pTransp, "SongSelectRT", "SongSelection\\s" & SongR  &".png",1,"{'mt':2,'color':0,'width':" & 18+width & ", 'height':" & 35+height & ", 'xpos':" & 45 - xval2 & " ,'ypos':" & 25 - yval & "}"
			End if 
		else 
	WriteToLog "     ", "tmrSongSelectAni_Timer:"&tmrSongSelectAni.UserValue
			puPlayer.LabelSet pTransp, "SongSelectLT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':18, 'height':31, 'xpos':4 ,'ypos':25}"
			puPlayer.LabelSet pTransp, "SongSelectCT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':28, 'height':48, 'xpos':19 ,'ypos':15}"
			puPlayer.LabelSet pTransp, "SongSelectRT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':18, 'height':31, 'xpos':45 ,'ypos':25}"	
			tmrSongSelectAni.Enabled=False 
			UpdateDMDSong
		End if 
	Else 									' Ball Launch Zoom Song Icon Animation
		xval1=tmrSongSelectAni.UserValue*1
		yval=tmrSongSelectAni.UserValue*.5
		height=tmrSongSelectAni.UserValue*2
		width=tmrSongSelectAni.UserValue
'WriteToLog "     ", "tmrSongSelectAni.UserValue:" & tmrSongSelectAni.UserValue
		if 19+xval1 < 30 then
			puPlayer.LabelSet pDMdFull, "SongSelectC", "SongSelection\\s" & Songnr  &".png",1,"{'mt':2,'color':0,'width':" & 28+width & ", 'height':" & 48+height & ", 'xpos':" & 19+xval1 & " ,'ypos':" & 15-yval & "}"
		elseif 19+xval1 > 35 then
			tmrSongSelectAni.Enabled = False
			puPlayer.LabelSet pDMdFull, "SongSelectC", "PupOverlays\\clear.png",1,""
			
			PupOverlayInGame
			pJackpotCounts False
			SetupEddieInserts
		End if 
	End if 

End Sub 


Sub SelectModeMusic(Mode)
	dim bModeMusic:bModeMusic=True 
WriteToLog "     ", "SelectModeMusic:" & Mode

	Dim SongIdx:SongIdx=0
    Select Case Mode
        Case kModeNOTB:SongIdx=1
        Case kModeCyborg:SongIdx=2
        Case kModeTrooper:SongIdx=3
        Case kMode2M2M:SongIdx=4
        Case kModeAces:SongIdx=5
        Case kModeMadness::SongIdx=6:bModeMusic=False
        Case kModeIcarus:SongIdx=7
        Case kModeHallowed::SongIdx=8
        Case kModeRime::SongIdx=9:bModeMusic=False
        Case kModeRTTH:SongIdx=10
        Case kModeFear:SongIdx=11
		case kModeMummy:SongIdx=0:bModeMusic=False			' TBD Update when I get the mode song
    End Select
	SelectMusic2 SongIdx, bModeMusic
End Sub 

Sub SelectMusic(SongIndex)
   SelectMusic2 SongIndex, False
End Sub 

Sub SelectMusic2(SongIndex, bModeMusic)
	dim ModeStr:ModeStr=""
WriteToLog "     ", "SelectMusic:" & SongIndex
	if bModeMusic then ModeStr="_Mode"

    DMDFlush
    Select Case SongIndex
        Case 0:DMD "", "", "DMD_Powerslave", eNone, eNone, eNone, 2000, True, "": playMusic "IM_Powerslave" & ModeStr & ".mp3"
        Case 1:DMD "", "", "DMD_Beast", eNone, eNone, eNone, 2000, True, ""		: playMusic "IM_Beast" & ModeStr & ".mp3"
        Case 2:DMD "", "", "DMD_Wasted", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Wasted" & ModeStr & ".mp3"
        Case 3:DMD "", "", "DMD_Trooper", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Trooper" & ModeStr & ".mp3"
        Case 4:DMD "", "", "DMD_2Minutes", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_2Minutes" & ModeStr & ".mp3"
        Case 5:DMD "", "", "DMD_Aces", eNone, eNone, eNone, 2000, True, "" 		: playMusic "IM_Aces" & ModeStr & ".mp3" 
        Case 6:DMD "", "", "DMD_Madness", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Madness" & ModeStr & ".mp3" 
        Case 7:DMD "", "", "DMD_Flight", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Flight" & ModeStr & ".mp3" 
        Case 8:DMD "", "", "DMD_Hallowed", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Hallowed" & ModeStr & ".mp3"
        Case 9:DMD "", "", "DMD_Rime", eNone, eNone, eNone, 2000, True, "" 		: playMusic "IM_Rime" & ModeStr & ".mp3"
        Case 10:DMD "", "", "DMD_Hills", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Hills" & ModeStr & ".mp3"
        Case 11:DMD "", "", "DMD_Fear", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Fear" & ModeStr & ".mp3"
        Case 12:DMD "", "", "DMD_Ghost", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Ghost" & ModeStr & ".mp3" 
        Case 13:DMD "", "", "DMD_Wicker", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Wicker" & ModeStr & ".mp3" 
        Case 14:DMD "", "", "DMD_Speed", eNone, eNone, eNone, 2000, True, "" 	: playMusic "IM_Speed" & ModeStr & ".mp3"
		Case 15:DMD "", "", "DMD_nomusic", eNone, eNone, eNone, 2000, True, "" 	: Playclear pMusic
    End Select	
End Sub 

Sub PlayRandomMusic
    Songnr = INT(RND * kMaxSongs)
End Sub

Sub UpdateDMDSong() 'Updates the DMD with the chosen song
	Dim SongL:SongL=Songnr-1
	Dim SongR:SongR=Songnr+1
	if SongL<0 then SongL=kMaxSongs
	if SongR>kMaxSongs then SongR=0

	SelectMusic(Songnr)

	puPlayer.LabelSet pTransp, "SongSelectLT", "SongSelection\\s" & SongL  &".png",1,"{'mt':2,'color':0,'width':18, 'height':31, 'xpos':4 ,'ypos':25}"
	puPlayer.LabelSet pDMdFull, "SongSelectC", "SongSelection\\s" & Songnr  &".png",1,"{'mt':2,'color':0,'width':28, 'height':48, 'xpos':19 ,'ypos':15}"
	puPlayer.LabelSet pTransp, "SongSelectRT", "SongSelection\\s" & SongR  &".png",1,"{'mt':2,'color':0,'width':18, 'height':31, 'xpos':45 ,'ypos':25}"
	Puplayer.SendMsg "{ ""mt"":301, ""SN"": " & pTransp &", ""FN"":6 }"			' Bring to Front 
	
End Sub

Sub ShowSongSelect()
WriteToLog "     ", "ShowSongSelect:" & PlayersPlayingGame

	SceneGeneralStart pDMDFull, True, False, "SongSelection", "SongSelect.mp4", "I:SongSelection\\UseFlippers.png^^^^^^^^^", "^^^^^^^^^"
	bSongSelect=True
	if balls=1 and bInstantInfo=False then 					' 1st ball randomly pick a song, unless we are on instant info
		Songnr = INT(RND * kMaxSongs)
	End if 
	UpdateDMDSong	
	If PlayersPlayingGame = 1 Then 
		pupevent 511
	ElseIf PlayersPlayingGame = 2 Then
		pupevent 521
	ElseIf PlayersPlayingGame = 3 Then
		pupevent 531
	ElseIf PlayersPlayingGame = 4 Then
		pupevent 541
	End If

End Sub

Sub StopSongSelect()
	bSongSelect=False

	puPlayer.LabelSet pTransp, "SongSelectLT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':18, 'height':35, 'xpos':4 ,'ypos':25}"
	puPlayer.LabelSet pTransp, "SongSelectCT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':28, 'height':48, 'xpos':19 ,'ypos':15}"
	puPlayer.LabelSet pTransp, "SongSelectRT", "PupOverlays\\clear.png",1,"{'mt':2,'color':0,'width':18, 'height':35, 'xpos':45 ,'ypos':25}"	
	SceneClearLabels

	tmrSongSelectAni_Type=1
	tmrSongSelectAni.UserValue = 0
	tmrSongSelectAni.Interval=50
	tmrSongSelectAni.Enabled = True 

End Sub 

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetGILightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer 'not used in this table
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub


Sub GILightRed(bOn)
	Dim item
	For each item in GI_Top_Red_Col
		if bOn then 
			Item.state=1
		Else 
			Item.state=0
		End if 
	Next 
	For each item in GI_Bot_Red_Col
		if bOn then 
			Item.state=1
		Else 
			Item.state=0
		End if 
	Next 
End Sub 

Sub GILightWhite(bOn)
	Dim item
	For each item in GI_Top_White_Col
		if bOn then 
			Item.state=1
		Else 
			Item.state=0
		End if 
	Next 
	For each item in GI_Bot_White_Col
		if bOn then 
			Item.state=1
		Else 
			Item.state=0
		End if 
	Next 
End Sub 

Sub GIWall(bOn)		' This could be better but good enough for now 
	if bOn then 
		cab_gion.Image= "GION_cab"
	Else 
		cab_gion.Image= "GIOFF_cab"
	End if 
End Sub 

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aGiFlashers
        bulb.Visible = 1
    Next
	GILightWhite True
	GILightRed True

	BeaconDomeBaseDL = 0.7
	fBase7A001.BlendDisableLighting = BeaconDomeBaseDL
	PrimSpinner001.Image = "GION_g01"
	PrimSpinner002.Image = "GION_g01"
'	PrimLeftFlipper1.Image = "GION_g01"
'	PrimRightFlipper1.Image = "GION_g01"
'	PrimFlipperL.Image = "GION_g01"
'	PrimFlipperR.Image = "GION_g01"
'	g01_gion.Image = "GION_g01"
	wallRampOpenPrim.Image = "GION_ramps02"
'	ramps_gion.Image = "GION_ramps02"
	primRamp007.Image = "GION_ramps02"
'	backwall_gion.Image = "GION_backwall"
	PharaohHeadPrim.Image = "GION_backwall"
'	GuardianPrim="Guardian"
	GuardianPrim.Image="GION_backwall"
'	plastics_low_gion.Image = "GION_plastics04"
'	plastics_high_gion.Image = "GION_plastics04"
'	toys_gion.Image = "GION_toys"
'	bumpers_gion.Image = "GION_bumpers"
	balltrapMetal_gion.Image="GION_BallTrap02"
	balltrapPlastics_gion.Image="GION_BallTrap02"
	target004p.Image = "GION_g01"
	target005p.Image = "GION_g01"
	target006p.Image = "GION_g01"
	cab_gion.Image= "GION_cab"
	plasticRoof.Visible=True
	primPost001.Image = "GION_mbPost"
	primPost002.Image = "GION_mbPost"


	primNBall1.blenddisablelighting=3		' Make newton balls brighter 
	primNBall2.blenddisablelighting=3		' Make newton balls brighter 
'	primNBall1.Image = "GION_newtonBalls"
'	primNBall2.Image = "GION_newtonBalls"


	FlasherPF.Opacity=100
	g01_gion.Opacity=0:				g01_gioff.Visible = True
	plastics_low_gion.Opacity=0:	plastics_low_gioff.Visible = True
	plastics_high_gion.Opacity=0:	plastics_high_gioff.Visible = True
	ramps_gion.Opacity=0:			ramps_gioff.Visible = True
	backwall_gion.Opacity=0:		backwall_gioff.Visible = True
	PrimLeftFlipper1.Opacity=0:		PrimLeftFlipperoff.Visible = True
	PrimRightFlipper1.Opacity=0:	PrimRightFlipperoff.Visible = True
	PrimFlipperL.Opacity=0:			primFlipperLoff.Visible = True
	PrimFlipperR.Opacity=0:			primFlipperRoff.Visible = True
	bumpers_gion.Opacity=0:			bumpers_gioff.Visible = True
	toys_gion.Opacity=0:			toys_gioff.Visible = True 
	primNBall1.Opacity=0:			primNBall1off.Visible = True 
	primNBall2.Opacity=0:			primNBall2off.Visible = True 
	target004p.Opacity=0:			target004p_gioff.Visible = True 
	target005p.Opacity=0:			target005p_gioff.Visible = True 
	target006p.Opacity=0:			target006p_gioff.Visible = True 
'	cab_gion.Opacity=0:				cab_gioff.Visible = True 

	tmrGIFade.UserValue=1		' Fade Off 
	tmrGIFade.Interval = 6
	tmrGIFade.Enabled = True 

	LBumper2.state=1
	LBumper3.state=1


End Sub

	primBallLock1.Visible = False
	primBallLock2.Visible = False
	primBallLock3.Visible = False
	primBallLock4.Visible = False
	primBallLock5.Visible = False
	primBallLock6.Visible = False


Sub tmrGIFade_Timer()
'WriteToLog "     ", "GIFADE Opacity:" & g01_gion.Opacity
	if tmrGIFade.UserValue=0 then 			' Fade Off 
		g01_gion.Opacity=g01_gion.Opacity-2
		FlasherPF.Opacity=FlasherPF.Opacity+2
		plastics_low_gion.Opacity=plastics_low_gion.Opacity-2
		plastics_high_gion.Opacity=plastics_high_gion.Opacity-2
		ramps_gion.Opacity=ramps_gion.Opacity-2
		backwall_gion.Opacity=backwall_gion.Opacity-2
		PrimLeftFlipper1.Opacity=PrimLeftFlipper1.Opacity-2
		PrimRightFlipper1.Opacity=PrimRightFlipper1.Opacity-2
		PrimFlipperL.Opacity=PrimFlipperL.Opacity-2
		PrimFlipperR.Opacity=PrimFlipperR.Opacity-2
		bumpers_gion.Opacity=bumpers_gion.Opacity-2
		primNBall1.Opacity=primNBall1.Opacity-2
		primNBall2.Opacity=primNBall2.Opacity-2
		toys_gion.Opacity=toys_gion.Opacity-2

		target004p.Opacity=target004p.Opacity-2
		target005p.Opacity=target005p.Opacity-2
		target006p.Opacity=target006p.Opacity-2

'		cab_gion.Opacity=cab_gion.Opacity-2
		if g01_gion.Opacity=0 then 
			tmrGIFade.Enabled = False 
		End if 
	Else 		' Fade On 
		g01_gion.Opacity=g01_gion.Opacity+2
		FlasherPF.Opacity=FlasherPF.Opacity-2
		plastics_low_gion.Opacity=plastics_low_gion.Opacity+2
		plastics_high_gion.Opacity=plastics_high_gion.Opacity+2
		ramps_gion.Opacity=ramps_gion.Opacity+2
		backwall_gion.Opacity=backwall_gion.Opacity+2
		PrimLeftFlipper1.Opacity=PrimLeftFlipper1.Opacity+2
		PrimRightFlipper1.Opacity=PrimRightFlipper1.Opacity+2
		PrimFlipperL.Opacity=PrimFlipperL.Opacity+2
		PrimFlipperR.Opacity=PrimFlipperR.Opacity+2
		bumpers_gion.Opacity=bumpers_gion.Opacity+2
		primNBall1.Opacity=primNBall1.Opacity+2
		primNBall2.Opacity=primNBall2.Opacity+2
		toys_gion.Opacity=toys_gion.Opacity+2

		target004p.Opacity=target004p.Opacity+2
		target005p.Opacity=target005p.Opacity+2
		target006p.Opacity=target006p.Opacity+2

'		cab_gion.Opacity=cab_gion.Opacity-2
		if g01_gion.Opacity=100 then 
			g01_gioff.Visible = False
			plastics_low_gioff.Visible = False
			plastics_high_gioff.Visible = False
			ramps_gioff.Visible=False
			backwall_gioff.Visible=False
			tmrGIFade.Enabled = False 
			PrimLeftFlipperoff.Visible=False
			PrimRightFlipperoff.Visible=False
			PrimFlipperLoff.Visible=False
			PrimFlipperRoff.Visible=False
			bumpers_gioff.Visible=False
			toys_gioff.Visible=False 
			primNBall1off.Visible=False 
			primNBall2off.Visible=False 
'			cab_gioff.Visible=False 

			target004p_gioff.Visible=False 
			target005p_gioff.Visible=False 
			target006p_gioff.Visible=False 

		End if 
	End if 
	
End Sub 


Sub GiOff
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aGiFlashers
        bulb.Visible = 0
    Next


	BeaconDomeBaseDL = 0.2
	fBase7A001.BlendDisableLighting = BeaconDomeBaseDL
	PrimSpinner001.Image = "GIOFF_g01"
	PrimSpinner002.Image = "GIOFF_g01"
'	PrimLeftFlipper1.Image = "GIOFF_g01"
'	PrimRightFlipper1.Image = "GIOFF_g01"
'	PrimFlipperL.Image = "GIOFF_g01"
'	PrimFlipperR.Image = "GIOFF_g01"
'	g01_gion.Image = "GIOFF_g01"
	wallRampOpenPrim.Image = "GIOFF_ramps"
'	ramps_gion.Image = "GIOFF_ramps"
	primRamp007.Image = "GIOFF_ramps"
'	backwall_gion.Image = "GIOFF_backwall"
	PharaohHeadPrim.Image = "GIOFF_backwall"
'	GuardianPrim="Guardian"
	GuardianPrim.Image="GIOFF_backwall"
'	plastics_low_gion.Image = "GIOFF_plastics"
'	plastics_high_gion.Image = "GIOFF_plastics"
'	toys_gion.Image = "GIOFF_toys"
'	bumpers_gion.Image = "GIOFF_bumpers"
	 
	balltrapMetal_gion.Image="GIOFF_BallTrap01"
	balltrapPlastics_gion.Image="GIOFF_BallTrap01"
	target004p_gioff.Image = "GIOFF_g01"
	target005p_gioff.Image = "GIOFF_g01"
	target006p_gioff.Image = "GIOFF_g01"
	cab_gion.Image= "GIOFF_cab"
	plasticRoof.Visible=False 

'	primNBall1.Image = "GIOFF_newtonBalls"
'	primNBall2.Image = "GIOFF_newtonBalls"
	primPost001.Image = "GIOFF_mbPost"
	primPost002.Image = "GIOFF_mbPost"


	FlasherPF.Opacity=0:			FlasherPF.Visible=True
	g01_gion.Opacity=100:			g01_gioff.Visible = True
	plastics_low_gion.Opacity=100:	plastics_low_gioff.Visible = True
	plastics_high_gion.Opacity=100:	plastics_high_gioff.Visible = True
	ramps_gion.Opacity=100:			ramps_gioff.Visible = True
	backwall_gion.Opacity=100:		backwall_gioff.Visible = True
	PrimLeftFlipper1.Opacity=100:	PrimLeftFlipperoff.Visible = True
	PrimRightFlipper1.Opacity=100:	PrimRightFlipperoff.Visible = True
	PrimFlipperL.Opacity=100:		primFlipperLoff.Visible = True
	PrimFlipperR.Opacity=100:		primFlipperRoff.Visible = True
	bumpers_gion.Opacity=100:		bumpers_gioff.Visible = True
	toys_gion.Opacity=100:			toys_gioff.Visible = True 
	primNBall1.Opacity=100:			primNBall1off.Visible = True 
	primNBall2.Opacity=100:			primNBall2off.Visible = True 
'	cab_gion.Opacity=100:			cab_gioff.Visible = True 

	target004p.Opacity=100:			target004p_gioff.Visible = True 
	target005p.Opacity=100:			target005p_gioff.Visible = True 
	target006p.Opacity=100:			target006p_gioff.Visible = True 


	tmrGIFade.UserValue=0		' Fade Off 
	tmrGIFade.Interval = 6
	tmrGIFade.Enabled = True 

	LBumper2.state=0
	LBumper3.state=0


End Sub

' GI, light & flashers sequence effects
Dim LightSeqGi_Active:LightSeqGi_Active=False
Sub GiEffect(n)
	if LightSeqGi_Active then exit sub
	LightSeqGi_Active=True 
WriteToLog "     ", "GiEffect"
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 15
            LightSeqGi.Play SeqBlinking, , 20, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 15
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
    End Select
End Sub

Sub LightSeqGi_PlayDone()
	LightSeqGi_Active=False
End sub 

Sub LightEffect(n)   ' 0-Off, 1-Blink, 2-Random, 3-All BlinkFast, 4-Sequence Up, 5-Sequence Down
'WriteToLog "     ", "LightEffect"
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 15
            LightSeqInserts.Play SeqBlinking, , 20, 10
            PlaySoundVol "fx_thunder1", VolDef
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
            PlaySoundVol "fx_thunder2", VolDef
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
            PlaySoundVol "fx_thunder3", VolDef
        Case 4 ' seq up
            LightSeqInserts.UpdateInterval = 3
            LightSeqInserts.Play SeqUpOn, 15, 3
            PlaySoundVol "fx_thunder1", VolDef
        Case 5 'seq down
            LightSeqInserts.UpdateInterval = 3
            LightSeqInserts.Play SeqDownOn, 15, 3
            PlaySoundVol "fx_thunder1", VolDef
    End Select
End Sub


fBase7A001.Image = "domewhitebase"
dim CurrentBeaconLitColor, BeaconDomeBaseDL
BeaconDomeBaseDL = 0.5

fBase7A001.BlendDisableLighting = BeaconDomeBaseDL

Sub FlashBeacon(color, enabled)
	If Enabled then 
		FlashCycle=-1
		if color = "white" then
			Flasher005_5.Color = RGB(255,255,255)
			Flasher005.ImageA = "domewhiteflash"
'			fBase7A001.Image = "domewhitebase"
			fBase7A001.color = RGB(255,255,255)
			CurrentBeaconLitColor=rgb(255,220,180)
			fLit7A.Image = "domewhitelit"
			DOF 153, DOFpulse
		elseif color = "red" then 
			Flasher005_5.Color = RGB(255,0,0)
			Flasher005.ImageA = "domeredflash2"
'			fBase7A001.Image = "domeredbase"
			fBase7A001.color = RGB(255,230,230)
			CurrentBeaconLitColor=rgb(255,100,100)
			fLit7A.Image = "domeredlit"
			DOF 154, DOFpulse
		elseif color = "blue" then 
			Flasher005_5.Color = RGB(200,100,255)
			Flasher005.ImageA = "domeblueflash"
'			fBase7A001.Image = "domebluebase"
			fBase7A001.color = RGB(230,230,255)
			CurrentBeaconLitColor=rgb(100,100,255)
			fLit7A.Image = "domebluelit"
			DOF 156, DOFpulse
		elseif color = "green" then 
			Flasher005_5.Color = RGB(0,255,0)
			Flasher005.ImageA = "domegreenflash"
'			fBase7A001.Image = "domegreenbase"
			fBase7A001.color = RGB(230,255,230)
			CurrentBeaconLitColor=rgb(100,255,100)
			fLit7A.Image = "domegreenlit"
			DOF 155, DOFpulse
		elseif color = "cycle" then
			Flasher005_5.Color = RGB(255,0,0)
			Flasher005.ImageA = "domeredflash2"
'			fBase7A001.Image = "domeredbase"
			fBase7A001.color = RGB(255,230,230)
			CurrentBeaconLitColor=rgb(255,100,100)
			fLit7A.Image = "domeredlit"
			FlashCycle=1
			DOF 154, DOFpulse
		End If
		fLit7A.color = CurrentBeaconLitColor
		

		tmrRightFlash.Interval=200
		tmrRightFlash.Enabled = True
	else 
		Flasher005_5.Color = RGB(255,255,255)
		Flasher005.ImageA = "domewhiteflash"
		fBase7A001.Image = "domewhitebase"
		fLit7A.Image = "domewhitelit"
		tmrRightFlash.Enabled = False
	End if 
End Sub 

Sub FlashLightning()
	PlaySoundVol "fx_thunder1", VolSfx
	Flashforms Light072, 800, 50, 0
	Flashforms Light073, 800, 50, 0
	VPMtimer.AddTimer 1000, " Flashforms Light072, 700, 50, 0 '"
	VPMtimer.AddTimer 1000, " Flashforms Light073, 700, 50, 0 '"

	VPMtimer.AddTimer 1800, " Flashforms Light072, 800, 50, 0 '"
	VPMtimer.AddTimer 1800, " Flashforms Light073, 800, 50, 0 '"
End Sub 

Sub FlashLightning2()		' Flash and end blinkng 
	PlaySoundVol "fx_thunder1", VolSfx
	Flashforms Light072, 800, 50, 0
	Flashforms Light073, 800, 50, 0
	VPMtimer.AddTimer 1000, " Flashforms Light072, 700, 50, 0 '"
	VPMtimer.AddTimer 1000, " Flashforms Light073, 700, 50, 0 '"
	VPMtimer.AddTimer 1800, " SetSlowPulse light072:light072.state = 2 '"
	VPMtimer.AddTimer 1800, " SetSlowPulse light073:light073.state = 2 '" 
End Sub 

Dim FlashCycle
Dim FlashLevel5
Sub tmrRightFlash_Timer
	select case FlashCycle
		case 0:
			FlashBeacon "red", True
			FlashCycle=1
		case 1:
			FlashBeacon "white", True
			FlashCycle=2
		case 2:
			FlashBeacon "blue", True
			FlashCycle=0
	End Select 
	FlashLevel5 = 1 : Flasher005_Timer
End Sub 

Sub Flasher005_Timer
	dim flashx3, matdim
	If Flasher005.TimerEnabled = False Then 
		Flasher005.TimerEnabled = True
		Flasher005_5.TimerEnabled = True
		Flasher005.visible = 1
		Flasher005_5.visible = 1
		fLit7A.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasher005.opacity = 200 * flashx3
	Flasher005_5.opacity = 500 * flashx3
	fLit7A.BlendDisableLighting = 10 * flashx3
	fBase7A001.BlendDisableLighting = (4+(1-BeaconDomeBaseDL)) * flashx3 + BeaconDomeBaseDL
	UpdateMaterial "domelit9",0,0,0,0,0,0,flashx3,CurrentBeaconLitColor,0,0,False,True,0,0,0,0 
	FlashLevel5 = FlashLevel5 * 0.9 - 0.01
	If FlashLevel5 < 0.15 Then
		fLit7A.visible = 0
	Else
		fLit7A.visible = 1
	end If
	If FlashLevel5 < 0 Then
		Flasher005.TimerEnabled = False
		Flasher005_5.TimerEnabled = False
		Flasher005.visible = 0
		Flasher005_5.visible = 0
	End If
End Sub

Sub FlashEffect(n) 'adjusted for this table
WriteToLog "     ", "FlashEffect:" & n
    Select Case n
        Case 1:    ' all blink
            SetFlash Flasher004, 4, 2000, 50
'            SetFlash Flasher005, 5, 2000, 50

'            SetFlash Flasher003, 3, 2000, 50
			FlashFlasher1(5)
		
'            SetFlash Flasher002, 2, 2000, 50
'            SetFlash Flasher001, 1, 2000, 50
			FlashFlasher1(3)

'            SetFlash Flasher009, 9, 2000, 50
'            SetFlash Flasher010, 10, 2000, 50
        case 2: 'random
            SetFlash Flasher004, 4, 2000, 50
'            vpmtimer.addtimer 200, "SetFlash Flasher005,5,2000,50 '"

'            vpmtimer.addtimer 1200, "SetFlash Flasher003,3,2000,50 '"
            vpmtimer.addtimer 1200, "FlashFlasher1(5) '"

'            vpmtimer.addtimer 400, "SetFlash Flasher002,2,2000,50 '"

'            vpmtimer.addtimer 800, "SetFlash Flasher001,1,2000,50 '"
            vpmtimer.addtimer 800, "FlashFlasher1(3) '"


'            vpmtimer.addtimer 600, "SetFlash Flasher009,9,2000,50 '"
'            vpmtimer.addtimer 1000, "SetFlash Flasher010,10,2000,50 '"
        Case 1: ' all blink fast
            SetFlash Flasher004, 4, 1000, 50
'            SetFlash Flasher005, 5, 1000, 50

'            SetFlash Flasher003, 3, 1000, 50
			FlashFlasher1(5)

'            SetFlash Flasher002, 2, 1000, 50

'            SetFlash Flasher001, 1, 1000, 50
			FlashFlasher1(3)

'            SetFlash Flasher009, 9, 1000, 50
'            SetFlash Flasher010, 10, 1000, 50
        Case 4 ' seq up
            vpmtimer.addtimer 400, "SetFlash Flasher004,4,2000,50 '"
'            vpmtimer.addtimer 400, "SetFlash Flasher005,5,2000,50 '"

'            vpmtimer.addtimer 300, "SetFlash Flasher003,3,2000,50 '"
            vpmtimer.addtimer 300, "FlashFlasher1(5) '"

'            vpmtimer.addtimer 200, "SetFlash Flasher002,2,2000,50 '"
            vpmtimer.addtimer 200, "FlashFlasher1(3) '"
'            vpmtimer.addtimer 10, "SetFlash Flasher009,9,2000,50 '"
'            vpmtimer.addtimer 10, "SetFlash Flasher010,10,2000,50 '"
        Case 5 'seq down
            vpmtimer.addtimer 10, "SetFlash Flasher004,4,2000,50 '"
'            vpmtimer.addtimer 10, "SetFlash Flasher005,5,2000,50 '"

'            vpmtimer.addtimer 200, "SetFlash Flasher003,3,2000,50 '"
            vpmtimer.addtimer 200, "FlashFlasher1(5) '"

'            vpmtimer.addtimer 300, "SetFlash Flasher002,2,2000,50 '"
            vpmtimer.addtimer 300, "FlashFlasher1(3) '"
'            vpmtimer.addtimer 400, "SetFlash Flasher009,9,2000,50 '"
'            vpmtimer.addtimer 400, "SetFlash Flasher010,10,2000,50 '"
    End Select
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 42 'max ball velocity
ReDim rolling(tnob)
InitRolling

Dim BallDropCount
ReDim BallDropCount(tnob)

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
		if rolling(b) Then
			rolling(b) = False
			'StopSound("fx_ballrolling" & b)
			StopSound("BallRoll_" & b)
			StopSound("fx_metalrolling" & b)
		End if 
        aBallShadow(b).Y = 3000
		aBallShadow(b).Visible = False
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -24
		aBallShadow(b).Visible = True

        If BallVel(BOT(b))> 1 Then
'            If BOT(b).z <30 Then
'                ballpitch = Pitch(BOT(b) )
'                ballvol = Vol(BOT(b)) '*0.5
'            Else
'                ballpitch = Pitch(BOT(b) ) ' 25000 'increase the pitch on a ramp
'                ballvol = Vol(BOT(b)) * 10 '2
'            End If
            rolling(b) = True
			If BOT(b).z <30 Then
                StopSound("fx_metalrolling" & b)
				PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * (cVolTable + 0.1) * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else 
				StopSound("BallRoll_" & b)
				PlaySound("fx_metalrolling" & b), -1, VolPlayfieldRoll(BOT(b)) * (cVolTable + 0.1) * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			End if 

        Else
            If rolling(b) = True Then
                'StopSound("fx_ballrolling" & b)
				StopSound("BallRoll_" & b)
                rolling(b) = False
            End If
        End If

'        ' rothbauerw's Dropping Sounds
'        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
'            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
'        End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If BallDropCount(b) >= 5 Then
				BallDropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If BallDropCount(b) < 5 Then
			BallDropCount(b) = BallDropCount(b) + 1
		End If

    Next
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************


Sub SetLastSwitchHit(SwitchName)
	if bSkillshotReady and SwitchName<>"swPlungerRest" then ResetStandardSkillshot	' Switches cancel standard skillshot
	LastSwitchHit2=LastSwitchHit
	LastSwitchHit=SwitchName
End Sub 


' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

	tmrBallSearch.Interval = kBallSearchTimeout
	BallSearchCnt = 0
	tmrBallSearch.Enabled = True
	if kBallSearchEnabled=False then tmrBallSearch.Enabled = False 

    'resets the score display, and turn off attract mode
    StopAttractMode
'	pupevent 667

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 0
    PlayersPlayingGame = 1
	UpdatePlayers
    bOnTheFirstBall = True
    For i = 0 To MaxPlayers-1
		ScoreSave(i)=0
		LastScore(i)=0
        Score(i) = 0
		bReplayAwarded(i)=False
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
		PlayfieldMultiplierQual(i)=1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
		ExtraBallsLit(i) = 0
		TiltCount(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
	playclear pMusic
	PlaySoundVol "vo_Welcome2MyWorld", VolDef
    Game_Init()

	Scorbit.StartSession()
    ' you may wish to start some music, play a sound, do whatever at this point

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	bResetCurrentGame=False
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	Dim i
WriteToLog "     ", "ResetForNewPlayerBall"
    ' make sure the correct display is upto date
    AddScore 0

	pfxtimer.Enabled = 0

	ScoreSave(0)=""
	ScoreSave(1)=""
	ScoreSave(2)=""
	ScoreSave(3)=""
	UpdateBallsRemaining 0		' Update screen 
	AddCredit 0 				' Update Screen 


    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1:UpdateBonusxLights

    ' set the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

	bShatzEnabled=False	
    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False
	bSongSelect=True 
	PupOverlayScore()

	For i = 0 to kMaxLights-1		' No stacked modes on new ball
		StackedLightsIdx(i)=-1
	Next

    'Reset any table specific
    ResetNewBallVariables
    ResetNewBallLights()

	' Reset loop 
	LoopCount=0
	Loop2x=False 
	LoopTimer.Enabled = False 
	LoopTimer.UserValue=0
	UpdateLoopLights

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True
	bPlayfieldValidated = False

    'and the skillshot					 *-Good, +-Needs update, x-delete
    bSkillShotReady = True				'* Skillshot: SoftPlunge into the Skillshot Target [1M, +1 Eddie Letter, +5 Sec Ball Saver]
	bSuperSkillshotReady = False 		'* Super  SS: Hold Left flipper, Harder plunger to upper Left Flipper & then hit Super Skillshot (Dead End)  [ Qualify PFM, 5M, +1 Sec Ball Saver]
	bSuperSSkillshotsReady(0) = True 	'*1Super SSS: Soft Plunge to left outlane [ 20M, +10 Sec Ball Saver ]
	bSuperSSkillshotsReady(1) = True 	'*2Super SSS: Soft Plunge to left inlane  [  5M, +10 Sec Ball Saver ]
	bSuperSSkillshotsReady(2) = False 	'*3Super SSS: Full Plunge around loop and 3 orbits of UL Flupper [ 15M, +10 Sec Ball Saver] 
										'             **This has been disabled 
	bSuperSSkillshotsReady(3) = False  	'*4Super SSS: Hold Left Flipper, Full plunge, Hit Orb [ 10M, +3M Boost Power JP, +10Sec Ball Saver
	bSuperSSkillshotsReady(4) = True 	'*5Super SSS: Soft plunge to flipper (no switches) then Bullseye [5M, +5 Sec Ball Saver]
	bSuperSSkillshotsReady(5) = True	'*6Super SSS: Soft Plunge to left flipper, Right Orbit [8M, +5 Sec Ball Saver]
	bSuperSSkillshotOrbits=0

	ResetDrop

	UpdateModeLights		' Set the current mode correctly 
'Change the music ?
End Sub


Sub UpdateForMultiball(bEnabled)		' update the table for multiball mode 
WriteToLog "     ", "UpdateForMultiball:" & bEnabled
	if bEnabled then 					' Do whatever needs to be done to disable/enable things when switching in/out of Multiball
		TrooperDisable True			' Disable Trooper 
		if IsModeQual(kModeMummy) then EnableMummy False
	Else 
		TrooperDisable False 
		if IsModeQual(kModeMummy) then EnableMummy True
	End if 
End Sub 

Sub SetMultiballMode(bEnabled)
	if bMultiBallMode<> bEnabled then    ' Current Multiball is not in synch with Balls on Table
		bMultiBallMode = bEnabled
		UpdateForMultiball bEnabled 
	End if 
End Sub 

' Create a new ball on the Playfield
Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
	bCreatedBall = True
	ticksFromLastEject=0
    BallsOnPlayfield = BallsOnPlayfield + 1
    RandomSoundBallRelease BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multiball flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If (BallsOnPlayfield-BallsInRealLock)> 1 Then
        DOF 143, DOFPulse
		SetMultiballMode true 
        bAutoPlunger = True
	End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table
CreateMultiballTimer.Interval = 2000
Sub AddMultiball(nballs)
WriteToLog "     ", "AddMultiball:" & nballs
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub
Sub AddMultiballFast(nballs)
WriteToLog "     ", "AddMultiballFast"
	if CreateMultiballTimer.Enabled = False and ticksFromLastEject>200 then 
		CreateMultiballTimer.Interval = 100		' shortcut the first time through 
	End If 
   AddMultiball(nballs)
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
	CreateMultiballTimer.Interval = 2000
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield <MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

sub ResetBallSearch()
	if kBallSearchEnabled=False then Exit Sub 
	if BallSearchResetting then Exit Sub	' We are resetting jsut exit for now 
	'WriteToLog "     ", "Ball Search Reset"
	tmrBallSearch.Enabled = False	' Reset Ball Search
	BallSearchCnt=0
	tmrBallSearch.Enabled = True
End Sub 
dim BallSearchResetting:BallSearchResetting=False

Sub tmrBallSearch_Timer()	' We timed out
	dim Ball
	Dim BallStr
	Dim NumBallsHere:NumBallsHere=0

	' See if we are in mode select, a flipper is up that might be holding the ball or a ball is in the lane 


	'tmrPauseTimers.Enabled = False and _     ' Not sure we want this one in the check in case the timers get disabled and a ball gets stuck

	if bGameInPlay and _
		bInService=False and _
		hsbModeActive = False  and _ 
		tmrBonusTotal.Enabled = False and _
		bBallInPlungerLane = False and _
		tmrPauseTimers.Enabled = False and _
		LFPress <> 1 and _
		RFPress <> 1 and _ 
		LeftFlipper.CurrentAngle <> LeftFlipper.EndAngle and _
		RightFlipper.CurrentAngle <> RightFlipper.EndAngle Then

'		WriteToLog "     ", "--- listing balls ---"
		For each Ball in GetBalls
			if Ball.x<CapKicker2.x+1 and Ball.x>CapKicker2.x-1 and Ball.y<CapKicker2.y+1 and Ball.y>CapKicker2.y-1 Then
				' CapKicker2 Ball
			elseif Ball.x<CapKicker1.x+1 and Ball.x>CapKicker1.x-1 and Ball.y<CapKicker1.y+1 and Ball.y>CapKicker1.y-1 Then
				' CapKicker1 Ball
			else 
				NumBallsHere=NumBallsHere+1
				BallStr = "(x:" & Ball.x & " y:" & Ball.y & " z:" & Ball.z & ")"
'				WriteToLog "     ", "Ball (" & NumBallsHere & "):" & BallStr
			End if 
		Next
'		WriteToLog "     ", "--- listing balls ---"
		WriteToLog "     ", "Ball Search - NO ACTIVITY " & BallSearchCnt & " " & NumBallsHere

		PauseTimersForce 8000
		if BallSearchCnt >= 3 Then

'	TBD Might want to implement this
'			BallsOnPlayfield = 0
'			for each Ball in GetBalls
'				Ball.DestroyBall
'			Next

			BallSearchCnt = 0
			if BallsOnPlayfield > 0 then 	' somehow we might have drained and didnt catch it??
				BallsOnPlayfield = BallsOnPlayfield - 1  ' We cant find the ball (remove one)
			End if
			AddMultiball 1

			' Do Pup Animation
			QueueScene2 0, "SceneGeneralStart pDMDFull, True, False, ""Backglass"", ""BG cloud.mp4"", ""^^^LOCATING PINBALLS^^FAILED...^^^" & BallStr & "^"", ""^^^^^^^^^"" ", 3000, 1, True 
			QueueScene "SceneClearLabels", 0, 1
			vpmtimer.addtimer 3000, "BallSearchClear '"
			Exit sub
		End if

		' Do Pup Animation
		if BallSearchCnt = 2 and NumBallsHere=1 then 	' go ahead and move it 
			QueueScene2 0, "SceneGeneralStart pDMDFull, True, False, ""Backglass"", ""BG cloud.mp4"", ""^^^LOCATING PINBALLS^^CALLING OPERATOR...^^^" & BallStr & "^"", ""^^^^^^^^^"" ", 3000, 1, True 
			QueueScene "SceneClearLabels", 0, 1

			For each Ball in GetBalls
				if Ball.x<CapKicker2.x+1 and Ball.x>CapKicker2.x-1 and Ball.y<CapKicker2.y+1 and Ball.y>CapKicker2.y-1 Then
					' CapKicker2 Ball
				elseif Ball.x<CapKicker1.x+1 and Ball.x>CapKicker1.x-1 and Ball.y<CapKicker1.y+1 and Ball.y>CapKicker1.y-1 Then
					' CapKicker1 Ball
				else 
					Ball.x = 865
					Ball.y = 786
					Ball.z = 25
				End if 
			Next 
		Else 
			QueueScene2 0, "SceneGeneralStart pDMDFull, True, False, ""Backglass"", ""BG cloud.mp4"", ""^^^LOCATING PINBALLS^^PLEASE WAIT...^^^" & BallStr & "^"", ""^^^^^^^^^"" ", 3000, 1, True 
			QueueScene "SceneClearLabels", 0, 1
		End if

		' End Pup animation

		BallSearchResetting=True
		BallSearchCnt = BallSearchCnt + 1

		If BallsInRealLock=0 then ' No balls should be locked, Clear it
			vpmtimer.addtimer 2000, "RotateRamp " & gRampPos & " '"	' Rotate it back 
			RotateRamp(0)
		elseif gRampPos=2 then 
			vpmtimer.addtimer 2000, "RotateRamp " & gRampPos & " '"	' Rotate it back 
			RotateRamp(1)
		End if 
		Exit_Underworld isRampUp()
		CapKicker2a.Kick -8, 15
		vpmtimer.addtimer 3000, "BallSearchClear '"
	Else
		ResetBallSearch
	End if 
End Sub 

Sub BallSearchClear()
	BallSearchResetting = False
End Sub 

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
Sub EndOfBall()
WriteToLog "     ", "EndOfBall:" & Now
	pClearEverything2
	SceneClearLabels
	DOF 178, DOFpulse 'Drain Effect
	Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	If NOT Tilted Then
		If IsModeQual(kModeNOTB) then 
			EndNOTB False 
		Else 
			bBonusHeld = False
			BonusScore
		End if 
	Else 'if tilted then only add a short delay and go to the next end of ball routine.
		vpmtimer.addtimer 200, "EndOfBall2 : pupevent 512 '"
	End If
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier(CurrentPlayer) * ((SwitchBonusCount * 3500) + (MummyBonusCount * 25000) + (DeathblowBonusCount * 45000) + (LoopsBonusCount * 50000) + (GetPowerFeaturesComplete *75000) + (SoulShardCount(CurrentPlayer) * 250000) + (EDDIESCollectedBonusCount * 250000) )
End Function 

Sub ShowBonusLine(BonusName, Title, Cnt, Score)
	PlaySoundVol "fx_gunshot", VolSfx
	PuPlayer.LabelSet pDMDFull,"Lbl" & BonusName, Title, 1,""	
	PuPlayer.LabelSet pDMDFull,"Cnt" & BonusName, Cnt & " X", 1,""	
	PuPlayer.LabelSet pDMDFull,BonusName, FormatScore(Score), 1,""
End Sub 

Sub ShowTotalBonusLine(BonusName, Title, Cnt)	' Total Line shows 1X,2X..etc with score going up
	PlaySoundVol "sfx_gun14", VolSfx
	PuPlayer.LabelSet pDMDFull,BonusName, Title, 1,""

	' Add the bonus to the score
	if Cnt=-1 then 
		DMD CL(0, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), CL(1, FormatScore(BonusScoreTotal)), "", eNone, eNone, eNone, 2000, True, ""
	Else 
		PuPlayer.LabelSet pDMDFull,"BonusSwitches", 		FormatScore(3500*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusMummyLetters", 	FormatScore(25000*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusDeathblows", 		FormatScore(45000*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusLoops", 			FormatScore(50000*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusPowerFeatures",	FormatScore(75000*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusSoulShards", 		FormatScore(250000*Cnt), 1,""
		PuPlayer.LabelSet pDMDFull,"BonusEDDIESCollected", 	FormatScore(250000*Cnt), 1,""
	End if 
End Sub 

Sub BonusScore()
WriteToLog "     ", "BONUS SCORE:"
	QueueSetDefault 0, "", ""				' Disable for this scene
	pClearEverything2
	PupOverlayBonusSc
	SceneGeneralStart pDMDFull, False, False, "FearOfTheDark", "FearOfDarkLoop.mp4", "8:BONUS^^^^^^^^^", "^^^^^^^^^"
	tmrBonusTotal_Multiplier=0
	tmrBonusTotal_Skip=False
	tmrBonusTotal.Interval = 1000
	tmrBonusTotal.UserValue=0
	tmrBonusTotal.Enabled = True 

' Test Bonus Multiplier 	
'BonusMultiplier(CurrentPlayer)=5

End Sub

Dim tmrBonusTotal_Skip
Dim tmrBonusTotal_Multiplier
Sub tmrBonusTotal_Timer
	tmrBonusTotal.Interval = 100
	tmrBonusTotal.UserValue=tmrBonusTotal.UserValue+1
	Select case tmrBonusTotal.UserValue
		case 1:
			' Force Just in case 
			playmedia "FearOfDarkLoop.mp4", "FearOfTheDark", pDMDFull, "", -1, "", 1, 1
			ShowBonusLine "BonusSwitches",			"SWITCHES",	 				SwitchBonusCount, 				3500
		case 2:
			ShowBonusLine "BonusMummyLetters",		"MUMMY LETTERS COLLECTED", 	MummyBonusCount, 				25000
		Case 3: 
			ShowBonusLine "BonusDeathblows",		"DEATHBLOWS", 				DeathblowBonusCount, 			45000
		Case 4:
			ShowBonusLine "BonusLoops",				"LOOPS", 					LoopsBonusCount, 				50000
		Case 5:
			ShowBonusLine "BonusPowerFeatures",		"POWER FEATURES COLLECTED", GetPowerFeaturesComplete(), 	75000
		Case 6:
			ShowBonusLine "BonusSoulShards",		"SOUL SHARDS COLLECTED", 	SoulShardCount(CurrentPlayer), 	250000
		Case 7:
			ShowBonusLine "BonusEDDIESCollected",	"EDDIES COLLECTED", 		EDDIESCollectedBonusCount, 		250000
		Case 8: 
			if tmrBonusTotal_Multiplier < BonusMultiplier(CurrentPlayer) and BonusMultiplier(CurrentPlayer)>1 then 
				tmrBonusTotal.Interval = 400
				tmrBonusTotal.UserValue = 7
				tmrBonusTotal_Multiplier=tmrBonusTotal_Multiplier+1
				ShowTotalBonusLine "BonusTotal", tmrBonusTotal_Multiplier & "X", tmrBonusTotal_Multiplier
			else 
				tmrBonusTotal.Interval=0
			End if 
		Case 9:
			tmrBonusTotal.Interval = 500
			ShowTotalBonusLine "BonusTotal", FormatScore(BonusScoreTotal), -1
		case 10:
			StartBonusCountdown
	End Select 
	if tmrBonusTotal_Skip then tmrBonusTotal.Interval=1
End Sub 

Sub StartBonusCountdown
WriteToLog "     ", "StartBonusCountdown:" & BonusScoreTotal & " Score:" & Score(CurrentPlayer)
	PlaySoundVol "fx_planedive2", VolSfx
	tmrBonusCountdown_done=False
	tmrBonusCountdown_steps=BonusScoreTotal\60
	tmrBonusCountdown.UserValue = BonusScoreTotal
	tmrBonusCountdown.Interval = 70
	if tmrBonusTotal_Skip then tmrBonusCountdown.Interval=1
	tmrBonusCountdown.Enabled = True 
End Sub 

Dim tmrBonusCountdown_done
Dim tmrBonusCountdown_steps
Sub tmrBonusCountdown_Timer 
WriteToLog "     ", "tmrBonusCountdown_Timer:" & tmrBonusCountdown.UserValue
	Dim Value
	tmrBonusCountdown.UserValue=tmrBonusCountdown.UserValue-tmrBonusCountdown_steps
	if tmrBonusCountdown.UserValue < (-20*tmrBonusCountdown_steps) then
		tmrBonusCountdown.Enabled = False
		tmrBonusTotal.Enabled = False
WriteToLog "     ", "StartBonusCountdown END Score:" & Score(CurrentPlayer)
		SceneClearLabels
		pClearBonusTxt
		VPMTimer.Addtimer 100, "EndOfBall2 '"		' Allows pup time to process pClearBonusTxt
		Exit Sub 
	elseif tmrBonusCountdown.UserValue<=0 and tmrBonusCountdown_done=False then
		AddScore tmrBonusCountdown_steps+tmrBonusCountdown.UserValue			' Add Remaining 
		Value=0
		StopSound "fx_planedive2"
		PlaySoundVol "sfx_gun5", VolSfx

		tmrBonusCountdown.Interval = 70
		tmrBonusCountdown_done=True

	elseif tmrBonusCountdown.UserValue>0 then
		Value = tmrBonusCountdown.UserValue
		AddScore tmrBonusCountdown_steps
	End if 
	PuPlayer.LabelSet pDMDFull,"BonusTotal",FormatScore(Value),1, ""
End Sub 


Sub BonusSceneCallback	' FlipperSkip Callback
	' Flipper Skip
	if LFPress and RFPress then		' Skip to total
		if tmrBonusCountdown.Enabled then 
			tmrBonusCountdown.Enabled = False 
			tmrBonusCountdown.Interval = 1
			tmrBonusCountdown.Enabled = True
		elseif tmrBonusTotal.Enabled Then 
			tmrBonusTotal_Skip=True 
		End if 
	End if 
End Sub 


Sub PlayYouMatchedStart(MatchValue)
	dim bMatched
	Dim MatchStr:MatchStr="00" 
	if MatchValue>0 then MatchStr=MatchValue & ""

	bMatched=(MatchValue=(Score(CurrentPlayer) mod 100))
	if bMatched then 
		MatchStr=MatchStr & "g"
		VpmTimer.AddTimer 5000, "DoMatchAward() '"
	End if 

	pClearEverything
	puPlayer.LabelSet pDMDFull,"Credits"," " ,1,""
	puPlayer.LabelSet pDMDFull,"Ball"," ",1,""

	PupOverlayMatch
	PuPlayer.LabelShowPage pDMDFull, 1, 0, ""

	PlaySoundVol "Sfx_MatchB", VolSfx
	QueueFlush 0
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""MatchFreeGame"", ""Match" & MatchStr & ".mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ", 7334, 1
	QueueScene2 0, "if bShowMatch then EndOfGame() ", 0, 1, True 		' Allows them to skip the match scene 
End Sub 

Sub DoMatchAward()
WriteToLog "     ", "Match Award:"
	KnockerSolenoid : DOF 122, DOFPulse
	DOF 121, DOFPulse
'	Credits=Credits+1
	AddCredit 1
	' TBD: Check DMD Adjustments since I think you can also award ExtraBall
End Sub 


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL

	' Reset Just in case 
	BallsOutlaneDrainCnt=0
	BallsOutlaneDrainIgnoreCnt=0

	' Reset Tilt
    Tilted = False
    Tilt = 0
	TiltCount(CurrentPlayer)=0
	TiltDecreaseTimer.Enabled=False 
    DisableTable False 'enable again bumpers and slingshots
	EDDIESCollectedBonusCount=0
	PuPlayer.LabelSet pDMDFull,"BonusTotal", "", 1,""		' Clear the bonus

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'WriteToLog "     ", "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""
		PlaySoundVol "vo_shootagain", VolDef
		SceneGeneralStart pDMDFull, False, False, "Callouts", "QuickInfo.mp4", "I:Callouts\\ShootAgainP" & CurrentPlayer +1 & ".png^^^^^^^^^", "^^^^^^^^^"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ' Create a new ball in the shooters lane
        vpmtimer.AddTimer 2000, "ResetForNewPlayerBall():CreateNewBall() '"
    Else ' no extra balls

        'BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1
		UpdateBallsRemaining -1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'WriteToLog "     ", "No More Balls, High Score Entry"
			QueueSetDefault 0, "", ""				' Disable for End of Game 

			' Turn off DOF so we dont accidently leave it on
			RandomSoundFlipperDownLeft LeftFlipper:DOF 101, DOFOff
			LeftFlipper.RotateToStart
			LeftFlipper1.RotateToStart
			RandomSoundFlipperDownRight RightFlipper:DOF 102, DOFOff
			RightFlipper.RotateToStart
			RightFlipper1.RotateToStart

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        Else
            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer
	Dim Match

	WriteToLog "     ", "EndOfBallComplete:" & BallsRemaining(CurrentPlayer) & " " & BallsRemaining(NextPlayer) 

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer >= PlayersPlayingGame)Then
            NextPlayer = 0
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'WriteToLog "     ", "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

		bGameInPLay = False									' EndOfGame sets this but need to set early to disable flippers 
		bShowMatch = True
		Match=10 * INT(RND * 9)
		if RND<(DMDStd(kDMDStd_MatchPCT)/100.0) then Match = Score(CurrentPlayer) mod 100		' Force Match based on Percent Setup 
		PlayYouMatchedStart Match

		If bReplayAwarded(CurrentPlayer)=False then  		' Lower the replay value if they didnt get it
			ReplayValue=ReplayValue - INT(DMDStd(kDMDStd_DynReplayStart)*(DMDStd(kDMDStd_ReplayPct)/100))
			if ReplayValue < 15000000 then ReplayValue=DMDStd(kDMDStd_DynReplayStart)
			SaveValue TableName, "ReplayValue", ReplayValue	' SAVE 
		End if

    Else
        ' set the next player
		SaveCurrentPlayer
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

		RestoreCurrentPlayer

        ' AND create a new ball
        CreateNewBall()
        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            Select Case CurrentPlayer
                Case 0:DMD "", "", "DMD_P1", eNone, eNone, eNone, 1000, True, "":PlaySoundVol "vo_player1", VolDef
				pDMDShowBig "PLAYER 1", 2 ,RGB(255,255,255) 
                Case 1:DMD "", "", "DMD_P2", eNone, eNone, eNone, 1000, True, "":PlaySoundVol "vo_player2", VolDef
				pDMDShowBig "PLAYER 2", 2 ,RGB(255,255,255) 
                Case 2:DMD "", "", "DMD_P3", eNone, eNone, eNone, 1000, True, "":PlaySoundVol "vo_player3", VolDef
				pDMDShowBig "PLAYER 3", 2 ,RGB(255,255,255) 
                Case 3:DMD "", "", "DMD_P4", eNone, eNone, eNone, 1000, True, "":PlaySoundVol "vo_player4", VolDef
				pDMDShowBig "PLAYER 4", 2 ,RGB(255,255,255) 
            End Select
        Else
			PlaySoundVol "vo_youareup", VolDef
            DMD "", "", "DMD_P1", eNone, eNone, eNone, 1000, True, ""
        End If

    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    WriteToLog "     ", "End Of Game"

	CycleMummyInserts False
	bShowMatch = False
	PuPlayer.LabelShowPage pDMDFull, 4, 0, ""
	pClearEverything
	PupOverlayBonusSc
	PuPlayer.playlistplayex pDMdFull,"PupOverlays","clear1.png", 1, 1

    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then

    End If

	StopInstantInfo()
    bJustStarted = False
	bSongSelect=False 
    ' ensure that the flippers are down
	SolLFlipper 0
    SolRFlipper 0
    SolURFlipper 0
    SolULFlipper 0
	LFPress=0
	RFPress=0
	playclear pMusic

	Scorbit.StopSession Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame

'	DisableTable True	' Disable table 
	OpenRamp2 False, True 
	If BallsInRealLock<>0 then 
		RotateRamp(0)
	End if 

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
	
	AttractRndVideoIdx=0
	pCurAttractPos=0			' Start at HighScore
    StartAttractMode(False)
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

Sub UpdateBallsRemaining(Count)
	BallsRemaining(CurrentPlayer)=BallsRemaining(CurrentPlayer)+Count

	if pDMDCurPage <> pScores then Exit Sub
	If bShowMatch=False then 
		puPlayer.LabelSet pDMDFull,"Ball","BALL " & ""& balls ,1,""
	End if

End Sub 

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub DrainDummy_Hit()	' Call this to short cycle outlane drains 
WriteToLog "     ", "DrainDummy_Hit"
	BallsOutlaneDrainCnt=BallsOutlaneDrainCnt+1
	BallsOutlaneDrainIgnoreCnt=BallsOutlaneDrainIgnoreCnt+1
	Drain_Hit()
End Sub

Sub Drain_Hit()
	Dim tmpBallSaver
	dim bShortCycle:bShortCycle=False 
	MotorCheck=False 'Used for Sarcophagus gear motor checking
	' Handle Outlane BallSaves (REVIVE)
	tmpBallSaver=OutlaneBallsaveBuffer <> 0
	if OutlaneBallsaveBuffer>0 then OutlaneBallsaveBuffer = OutlaneBallsaveBuffer-1

	' Handle Short Cycle Ball Saves
	if BallsOutlaneDrainCnt>0 and bGameInPLay then 
		BallsOutlaneDrainCnt=BallsOutlaneDrainCnt-1
		bShortCycle=True 
	Else 
		' Destroy the ball
		Drain.DestroyBall
		BallsOnPlayfield = BallsOnPlayfield - 1
		if BallsOutlaneDrainIgnoreCnt>0 then 
			BallsOutlaneDrainIgnoreCnt=BallsOutlaneDrainIgnoreCnt-1
			' Not sure we really need to do this but if not balls are on the table and we are queued up to add a ball technically we need to turn off multiball 
			if BallsOnPlayfield-BallsInRealLock=1 and CreateMultiballTimer.Enabled=False then SetMultiballMode False ' bMultiBallMode=False
			Exit Sub 
		End if 
	End if
	If bGameInPLay = False Then 							'don't do anything, just delete the ball
		if bResetCurrentGame and BallsOnPlayfield=0 then 	' Everything has drained
			DisableTable False
		End if 
		Exit Sub 
	End if

    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'if Tilted the end Ball Mode
    If Tilted Then
		BallSaverTimerCancel
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active, or are there balls in the queue to be released
        If bBallSaverActive = True or tmpBallSaver or mBalls2Eject<>0 or bPlayfieldValidated=False Then

			if bSkillshotReady and bMultiBallMode=False and IsModeQual(kModeNOTB)=False and IsModeActive(kMode2M2M)=False and LastSwitchHit<>"swPlungerRest" then 
WriteToLog "     ", "ENDING Ballsaver;" & bMultiBallMode 
				BallSaverTimerExpired.UserValue = 0 			' End BallSaver
			End if 

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
			if bShortCycle then 
				AddMultiballFast 1
			Else 
				AddMultiball 1
			End if 
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
			if bBallSaverSkipAnimation or bMultiBallMode then  ' Dont do ball save animation 
				bBallSaverSkipAnimation=False 
			else
				PlaySoundVol "vo_shootagain", VolDef
				DMD "_", CL(1, "BALL SAVED"), "", eNone, eBlinkfast, eNone, 800, True, ""
				'pupevent 605
				QueueFlush 0 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""BallSave.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ", 5674, 1
			End if  
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
			StopEndOfBallModeEntendTime=0

            If(BallsOnPlayfield-BallsInRealLock = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    SetMultiballMode False 
                    ' you may wish to change any music over at this point and

                    ' turn off any multiball specific lights
                    ChangeGi white
                    'stop any multiball modes
					StopMultiballModes
                End If
            End If

			If(BallsOnPlayfield-BallsInRealLock = 0) Then QueueFlush 0		' No balls - Flush Queue before queueing up end of modes
			If(BallsOnPlayfield-BallsInRealLock <= 1) Then	' Stop MB's that have a Qual Mode
				if isModeActive(kModeMummy) then StopMummyMB
				if IsModeActive(kModeTrooper) then StopTrooperMB
				if IsModeActive(kModeCyborg) then StopCyborg
				if IsModeActive(kModeMadness) then StopMadness
				if IsModeActive(kModeRTTH) then StopRTTH
			End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield-BallsInRealLock = 0) and bWaitMummyClear=False Then	' Need to make sure we are not clearing the MummyLock
                ' End Mode and timers

                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
				StopEndOfBallModeEntendTime = getQueueTime(0)	' See if we need to let animations finish playing
				if StopEndOfBallModeEntendTime = 0 then StopEndOfBallModeEntendTime = 500
WriteToLog "     ", "Queue EndOfBall:" & StopEndOfBallModeEntendTime
                vpmtimer.addtimer StopEndOfBallModeEntendTime, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

Sub AutoPlungeDelayed()
	'WriteToLog "     ", "autofire the ball"
	PlungerIM.AutoFire
	DOF 121, DOFPulse	'autoplunger
	DOF 112, DOFPulse	'strobe
	bAutoPlunger = False
'	bAutoPlunged = True	
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit() ' This means the trigger is down because ball is resting on it - ANDREW
WriteToLog "     ", "ball in plunger lane: " & bAutoPlunger & " " & bCreatedBall & " " & LastSwitchHit
    bBallInPlungerLane = True
	MotorCheck=True 'Used for Sarcophogus Gear Motor Checking
	If bMultiBallMode=false and bAutoPlunger = false Then DOF 170, DOFOn End If 'MX ball waiting
	if bAutoPlunger=False and bCreatedBall = False and LastSwitchHit = "swPlungerRest" then	' If we didnt create a ball this must have gone up and back so reset skillshot 
WriteToLog "     ", "AUTOSET!!!!"
		bSkillShotReady=True
		ResetSkillShotTimer.Enabled=0
	Elseif bAutoPlunger=False and bCreatedBall = False and LastSwitchHit <> "swPlungerRest"  then 
WriteToLog "     ", "Ball fell into shooter - Autoplunging  " & bAutoPlunger & " " & bCreatedBall & " " & LastSwitchHit
		bAutoPlunger=True 
	End if 
	bCreatedBall=False

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'WriteToLog "     ", "autofire the ball"
        PlungerIM.AutoFire 
        DOF 121, DOFPulse
        SoundPlungerReleaseBall()
        bAutoPlunger = False
		bBallInPlungerLane=False
	Else
		ScorbitClaimQR(True)
	End If

    'Start the Selection of the song and skillshot if ready
    If bSkillShotReady Then
        UpdateSkillshot()
		ShowSongSelect
		pClearEverything
    End If
    ' show the message to shoot the ball in case the player has fallen sleep :)
    swPlungerRest.TimerEnabled = 1
    ' remember last trigger hit by the ball.
    SetLastSwitchHit "swPlungerRest"
	'pupDMDScore.Enabled = 1 'ANDREW

End Sub
' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()' This means the trigger is up because ball has been fired - ANDREW
	swPlungerRest.TimerEnabled = 0
    DMDScorenow
    bBallInPlungerLane = False
	ResetBallSearch()
	DOF 170, DOFoff 'turn off waiting indicator	
	If bMultiBallMode=false and bAutoPlunger = false Then DOF 171, DOFPulse End If 'MX ball streak 
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
		
		StopSongSelect
		ResetSkillShotTimer.UserValue=0
        ResetSkillShotTimer.Enabled = 1
		ScorbitClaimQR(False)
    End If

    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, or else it will reset the time period
    If (bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False) and bPlayfieldValidated Then
        EnableBallSaver BallSaverTime
    End If

    LightEffect 4
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds
Sub swPlungerRest_Timer
    Dim tmp
    DMDFlush
    tmp = INT(RND * 4)
    Select case tmp
        case 0:DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 1500, True, "":PlaySoundVol "vo_firethecannon", VolDef
        case 1:DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 1500, True, "":PlaySoundVol "vo_showtime", VolDef
        case 2:DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 1500, True, "":PlaySoundVol "vo_thehangmaniseager", VolDef
        case 3:DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 1500, True, "":PlaySoundVol "vo_whatsthematteredballsfinallydrop", VolDef
    End Select
End Sub


' *************************************************************
' DAPHISHBOWL BallSaver and Timer Pause routines - This pauses all countdon timers when ball is in bumpers or captured somewhere.
Sub EnableBallSaver(seconds)
	EnableBallSaverGrace seconds, True, False
End Sub 

Sub EnableBallSaverGrace(seconds, bGracePeriod, bExtend)
	Dim currentVal:currentVal=0
WriteToLog "     ", "Ballsaver started " & seconds
    ' set our game flag
	bBallSaverGrace = bGracePeriod
    bBallSaverActive = True
    bBallSaverReady = False
	bBallSaverSkipAnimation = False

	if bExtend then 
		if BallSaverTimerExpired.Enabled then 
			currentVal=BallSaverTimerExpired.UserValue
		End if 
	End if 

    ' start the timer
	BallSaverTimerExpired.Interval = 10
	if bBallSaverGrace then 
		BallSaverTimerExpired.UserValue = currentVal + (1000 * (seconds+2))  ' 2 second grace period 
	Else 
		BallSaverTimerExpired.UserValue = currentVal + (1000 * seconds)
	End if 
	BallSaverTimerExpired.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
	PauseTimers(-1)				' Go ahead and do 1 extend 
End Sub

Sub IncreaseBallSaver(seconds)

	' Do ball saver check here
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If

	BallSaverTimerExpired.UserValue = BallSaverTimerExpired.UserValue + (1000 * (seconds))

	WriteToLog "     ", "IncreaseBallSaver: " & BallSaverTimerExpired.UserValue
End Sub 

Sub BallSaverTimerExpired_Timer()
	if tmrPauseTimers.Enabled then exit sub ' Dont change timer

	BallSaverTimerExpired.UserValue = BallSaverTimerExpired.UserValue - BallSaverTimerExpired.Interval 	' Drop 500 msec 
'WriteToLog "     ", "BallSaveCountDown " & BallSaverTimerExpired.UserValue & " " & Now
	if BallSaverTimerExpired.UserValue <= 0 or (bBallSaverGrace=False and BallSaverTimerExpired.UserValue < 2000) then				' Stop BallSaver  
WriteToLog "     ", "Ballsaver ended"
		BallSaverTimerExpired.Enabled = False
		BallSaverTimerExpired.UserValue=0
		bBallSaverActive = False
		bBallSaverGrace=True 
		if ExtraBallsAwards(CurrentPlayer)<>0 then 
			LightShootAgain.State = 1									' They have extra ball, go solid 
		else 
			LightShootAgain.State = 0									' Make sure the light is off 
		End if 
		BallSaverTime = DMDStd(kDMDStd_BallSave)

	elseif BallSaverTimerExpired.UserValue < 2000 then			' Just turn off the light but we still have 2 seconds left 
		if ExtraBallsAwards(CurrentPlayer)<>0 then 
			LightShootAgain.State = 1									' They have extra ball, go solid 
		else 
			LightShootAgain.State = 0									' Make sure the light is off 
		End if 

	elseif BallSaverTimerExpired.UserValue < 4000 and LightShootAgain.BlinkInterval <> 80 then
'WriteToLog "     ", "BallSaver Speedup"
		LightShootAgain.BlinkInterval = 80
		LightShootAgain.State = 2
	End If 

End Sub


Sub BallSaverTimerCancel()
WriteToLog "     ", "Ballsaver Cancel"
	BallSaverTimerExpired.Enabled = False
	bBallSaverActive = False
	bBallSaverGrace = True 
    ' if you have a ball saver light then turn it off at this point
	If(ExtraBallsAwards(CurrentPlayer) = 0)Then
		LightShootAgain.State = 0
	Else 
		LightShootAgain.State = 1
	End if 
	BallSaverTime = DMDStd(kDMDStd_BallSave)	
	BallSaverTimerExpired.UserValue=0
End Sub 


Sub PauseTimers(mSec)	' Pause the timers, Ignored During MB
	If (tmrPauseTimers.Enabled=False or tmrPauseTimers.UserValue<=10) and bMultiBallMode=False then		' Allow to overlap by 100msec
		PauseTimersForce mSec 
	End if
End Sub 

Sub PauseTimersForce(mSec)
	dim PauseTime

	tmrPauseTimers.Enabled=False 
	if mSec=-1 then 
		PauseTime=DMDStd(kDMDStd_BallSaveExtend)
	Else
		PauseTime=mSec
	End if
	tmrPauseTimers.Interval=10
	tmrPauseTimers.UserValue=PauseTime/tmrPauseTimers.Interval
	tmrPauseTimers.Enabled = True
'WriteToLog "     ", "PauseTimers: " & tmrPauseTimers.UserValue & " " & Now
End Sub 

Sub tmrPauseTimers_Timer()
	tmrPauseTimers.UserValue=tmrPauseTimers.UserValue-1
'WriteToLog "     ", "PauseTimers: " & tmrPauseTimers.UserValue & " " & Now
	if tmrPauseTimers.UserValue<=0 then 
'WriteToLog "     ", "UNPAUSE Timers: " 
		tmrPauseTimers.Enabled = False
		tmrPauseTimers.UserValue=0
	End if 
End Sub 

Sub PauseTimersCancel()
	tmrPauseTimers.Enabled = False
	tmrPauseTimers.UserValue=0
End Sub 


' DAPHISHBOWL BallSaver and Timer Pause routines - This pauses all countdon timers when ball is in bumpers or captured somewhere.
' *************************************************************


Dim FlashRampIdx:FlashRampIdx=76
Dim FlashRampState:FlashRampState=0

Dim FlashGuardianIdx:FlashGuardianIdx=75
Dim FlashGuardianState:FlashGuardianState=0
Dim tmrFlashGuardian_2x:tmrFlashGuardian_2x=0		' Slower
Sub tmrFlashGuardian_Timer()
	if bGameInPlay then 
		tmrFlashGuardian_2x=(tmrFlashGuardian_2x+1) MOD 4
		if FlashGuardianState = 2 then 
			if Lampz.State(FlashGuardianIdx) then 
				Lampz.SetLamp FlashGuardianIdx, False
			Else
				Lampz.SetLamp FlashGuardianIdx, True
			End If
		elseif FlashGuardianState = 1 then 
			Lampz.SetLamp FlashGuardianIdx, True
		elseif FlashGuardianState = 0 then
			Lampz.SetLamp FlashGuardianIdx, False
		End if


		if FlashRampState = 2 then
			if tmrFlashGuardian_2x=0 then 					' Flash slower
				Lampz.FadeSpeedUp(FlashRampIdx) = 0.05
				Lampz.FadeSpeedDown(FlashRampIdx) = 0.05 

				if Lampz.State(FlashRampIdx) then 
					Lampz.SetLamp FlashRampIdx, False
				Else
					Lampz.SetLamp FlashRampIdx, True
				End If
			End if 
		elseif FlashRampState = 1 then 
			Lampz.SetLamp FlashRampIdx, True
		elseif FlashRampState = 0 then
			Lampz.SetLamp FlashRampIdx, False
		End if

	End if 

End Sub 


'*********************
' Playfield Multiplier
'*********************
Sub CheckPFx(index) 'check target hits to activate the playfield multiplier
WriteToLog "     ", "CheckPFx " & index 
	Dim bHit:bHit=False

	if IsModeQual(kModeCyborg) then Exit Sub 	' Not active during Cyborg 
 
	Select case index 
		case -1: 	' Spot a shot 
			if lTargetX1.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX1.state = 1
				bHit=True
			elseif lTargetX2.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX2.state = 1
				bHit=True 
			elseif lTargetX3.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX3.state = 1
				bHit=True 
			elseif lTargetX4.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX4.state = 1
				bHit=True 
			End if 
		Case 0: ' Bumpers Target003
			if lTargetX1.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX1.state = 1
				bHit=True 
			End if 
		Case 1: ' Skillshot Target001
			if lTargetX2.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX2.state = 1
				bHit=True 
			End if 
		Case 2: ' Loop Target007
			if lTargetX3.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX3.state = 1
				bHit=True 
			End if 
		Case 3: ' SJP Target008
			if lTargetX4.state=2 Then
				if PlayfieldMultiplierQual(CurrentPlayer)<>3 then lTargetX4.state = 1
				bHit=True 
			End if 
	End Select 

	if bHit then 
WriteToLog "     ", "bHit: " & pfxtimer.Enabled
		PFXAddTime

		If pfxtimer.Enabled=False then	' Only Reset when starting
			if lTargetX1.state=1 and lTargetX2.state=1 and lTargetX3.state=1 and lTargetX4.state=1 then 
				QualPFM 1
				ResetPFxTargetLights
			End if 
		End if 
	End if 

End Sub

Sub PFXAddTime()				' Add time to PFX if we are running 
	If pfxtimer.Enabled then 
WriteToLog "     ", "PFX Add Time"
		' Add 5 seconds to timer 
		PFxSeconds = PFxSeconds+5
		light012.blinkinterval = 250
		light013.blinkinterval = 250		' Slow down since we are > 2 seconds now
	End if 
End Sub 

Sub ResetPFxTargetLights
WriteToLog "     ", "ResetPFxTargetLights"
    PFxCount = 0
    lTargetX1.State = 2
    lTargetX2.State = 2
    lTargetX3.State = 2
    lTargetX4.State = 2
End Sub

Sub RotateActivateX
    Dim tmp
    tmp = light001.State
    light001.State = light002.State
    light002.State = tmp
End Sub

Sub UpdateBonusXLights
' Update the lights
'Select Case BonusMultiplier(CurrentPlayer)
'Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
'Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
'Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
'Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
'Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
'End Select
End Sub

Sub QualPFM(n)
	WriteToLog "     ", "QualPFM:" & PlayfieldMultiplierQual(CurrentPlayer)
	If pfxtimer.Enabled then 	
		' Timer is already running we just add the 5 seconds 
	else 
		if(PlayfieldMultiplierQual(CurrentPlayer) < MaxMultiplier) then		' QUAL PFM
			PlayfieldMultiplierQual(CurrentPlayer)=PlayfieldMultiplierQual(CurrentPlayer)+1
			if PlayfieldMultiplierQual(CurrentPlayer)=2 then PlaySoundVol "vo_2x", VolDef
			if PlayfieldMultiplierQual(CurrentPlayer)=3 then PlaySoundVol "vo_3x", VolDef
			UpdatePFXLights
		End if 
	End if 
End Sub 

Sub StartPFMCheck(index)
WriteToLog "     ", "StartPFM: L1:" & light001.State & "L2:" & light002.State & " " & index
	if (light001.State=1 and index=1) or (light002.State=1 and index=2) then
		StartPFM
	End If
End Sub 

Sub StartPFM()
Dim PFMClip
Dim PFMcallout
WriteToLog "     ", "StartPFM"
	PlayfieldMultiplier(CurrentPlayer)=PlayfieldMultiplierQual(CurrentPlayer)
	PlayfieldMultiplierQual(CurrentPlayer)=1
	PFxActivated = 1
	PFxSeconds = 20
	pfxtimer.Interval = 1000
	pfxtimer.Enabled = 1
	light001.State = 0
	light002.State = 0
	if PlayfieldMultiplier(CurrentPlayer)=2 then 
		light013.State = 2
		light013.blinkinterval = 250
	Else 
		light012.State = 2
		light012.blinkinterval = 250
	End if 

'	PlaySoundVol "Fanfare5", VolSfx
'	If PlayfieldMultiplier(CurrentPlayer)= 1 Then pupevent 616		' When is this USED ???
	If PlayfieldMultiplier(CurrentPlayer)= 2 Then PFMClip="Playfield multiplier 2x.mp4":PFMcallout="vo_2xActivated"
	If PlayfieldMultiplier(CurrentPlayer)= 3 Then PFMClip="Playfield multiplier 3x.mp4":PFMcallout="vo_3xActivated"

	PlaySoundVol PFMcallout, VolDef
	PlaySoundVol "sfx_PFMx", VolSfx

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", """ & PFMClip & """, ""^^^^^^^^^"", ""^^^^^^^^^"" ", 2300, 1
	QueueScene "SceneClearLabels", 0, 1

End Sub 

Sub AddPlayfieldMultiplier(n)
	PlayfieldMultiplierQual(CurrentPlayer)=n+1
	StartPFM
End Sub

Sub pfxtimer_Timer
	if tmrPauseTimers.Enabled then exit sub

'    WriteToLog "     ", "pfxtimer:" & PFxSeconds
    PFxSeconds = PFxSeconds - 1
    Select Case PFxSeconds
        Case 10:
			if PlayfieldMultiplier(CurrentPlayer)=2 then 
				light013.blinkinterval = 125
			else 
				light012.blinkinterval = 125
			End if 
        Case 0:
            PlayfieldMultiplier(CurrentPlayer) = 1
            PFxActivated = 0
            pfxtimer.Enabled = 0

			if lTargetX1.state=1 and lTargetX2.state=1 and lTargetX3.state=1 and lTargetX4.state=1 then 
				ResetPFxTargetLights
			End if 

            UpdatePFxLights
    End Select
End Sub

Sub UpdatePFxLights

	if pfxtimer.Enabled=False then 		' Not Activated Yet 
		if PlayfieldMultiplierQual(CurrentPlayer)=2 and light001.State=0 and light001.State=0 then 
			light001.State=1
			light013.State = 1
		elseif PlayfieldMultiplierQual(CurrentPlayer)=3 then 
			light001.State=1
			light002.State=1

			light013.State = 0
			light012.State = 1

		elseif PlayfieldMultiplierQual(CurrentPlayer)=1 then 
			light013.State = 0	'  2x
            light013.blinkinterval = 250
			light012.State = 0	'  3x
            light012.blinkinterval = 250
            light001.State = 0	' LeftInlane X 
			light002.State = 0	' RightInlane X 
		End if 
	Else 			' PFM Is Active 
		
	End if 
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

Sub AddCredit(Count)
	Credits=Credits+Count

	if pDMDCurPage <> pScores then Exit Sub
	If bShowMatch then Exit Sub 

	if bFreePlay then 
		puPlayer.LabelSet pDMDFull,"Credits","FREE PLAY " ,1,""	
	Else 
		puPlayer.LabelSet pDMDFull,"Credits","CREDITS " & ""& Credits ,1,""
	End if 
End Sub 


Sub AddScoreMode(mode, points)		' Add score to the Mode Points and really add score * PlayfieldMultiplier
	' TBD Include Bonus Multipliers 
	AddScore points					' PF Multipliers are included in this already 
	ModePoints(CurrentPlayer, Mode)=ModePoints(CurrentPlayer, Mode)+(points*PlayfieldMultiplier(CurrentPlayer))
End Sub 

Sub AddJPMode(mode, points)
	' TBD Include Bonus Multipliers 
	ModeJPPoints(CurrentPlayer, Mode)=ModeJPPoints(CurrentPlayer, Mode)+(points* PlayfieldMultiplier(CurrentPlayer))
End Sub 



Sub AddScore(points)		' Add points to the score AND update the score board * PlayfieldMultiplier
	ResetBallSearch()
    If Tilted Then Exit Sub

	' Replay
	if Score(CurrentPlayer)>=ReplayValue and bReplayAwarded(CurrentPlayer)=False then
		' Award Replay and increment value 
		ReplayValue=DMDStd(kDMDStd_DynReplayStart) + ReplayValue
		SaveValue TableName, "ReplayValue", ReplayValue	' SAVE 

		bReplayAwarded(CurrentPlayer)=True 
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""Replay.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ", 5000, 1
		If DMDStd(kDMDStd_ReplayType)=1 then 
			DoMatchAward
		Else 
			AwardExtraBall
		End if 
	End if 
WriteToLog "     ", "AddScore: " & points
    ' add the points to the current players score variable
    If PFxActivated Then
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    Else
        Score(CurrentPlayer) = Score(CurrentPlayer) + points
    End if

	Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted, but this doesn't have to be the case
    If Tilted Then Exit Sub
    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
'DMD "_", CL(1, "INCREASED JACKPOT"), "", eNone, eNone, eNone, 800, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000) Then
'		Jackpot = 6000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If(Tilted = False)Then
    End if
    SuperJackpot(CurrentPlayer) = SuperJackpot(CurrentPlayer) + points
End Sub


Sub AddBonusMultiplier(n)
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + n
        UPdateBonusXLights
        DMD "_", CL(1, "BONUS X IS " &BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, "":PlaySoundVol "fanfare8", VolDef
    Else
        AddScore 50000
        DMD "_", CL(1, "50000 POINTS"), "", eNone, eNone, eNone, 1500, True, ""
    End if
End Sub


Sub LightExtraBall()
	Dim Clip
	ExtraBallsLit(CurrentPlayer)=ExtraBallsLit(CurrentPlayer)+1
	lExtraBall.state = 1

	Clip="EBIsLit.mp4"
	If DMDFet(kDMDFet_FamilyMode) Then Clip="EBIsLit_F.mp4"

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Extraball"", """ & Clip & """, ""^^^^^^^^^"", ""^^^^^^^^^"" ", 1, 1
	QueueScene "PlaySoundVol ""sfx_ExtraBallLit"", VolSfx ", 1, 1
	QueueScene "PlaySoundVol ""vo_extraballLit1"", VolDef ", 4500, 1
	QueueScene "SceneClearLabels", 0, 1

End Sub 

Sub AwardExtraBall()
	Dim Clip
'    If NOT bExtraBallWonThisBall Then
		KnockerSolenoid
        DMD "_", CL(1, ("EXTRA BALL WON")), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("", 122, DOFPulse, DOFKnocker)

'		pupevent 611
		Clip="EBAward.mp4"
		If DMDFet(kDMDFet_FamilyMode) Then Clip="EBAward_F.mp4"

		QueueFlush 0
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""Extraball"", """ & Clip & """, ""^^^^^^^^^"", ""^^^^^^^^^"" ", 1, 1
		QueueScene "PlaySoundVol ""sfx_ExtraBall"", VolSfx ", 1, 1
		QueueScene "PlaySoundVol ""vo_extraball" & INT(RND*2)+1 & """, VolDef ", 5267, 1
		QueueScene "SceneClearLabels", 0, 1

        DOF 121, DOFPulse
		DOF 180, DOFPulse 'Additional EB effects 
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        if BallSaverTimerExpired.Enabled=False then 	'light the shoot again lamp only if Ball saver isnt running 
			LightShootAgain.State = 1 
		End if 
        GiEffect 2
        LightEffect 2

		ExtraBallsLit(CurrentPlayer)=ExtraBallsLit(CurrentPlayer)-1		' Deduct lit Extra balls 
		if ExtraBallsLit(CurrentPlayer)>0 then 
			lExtraBall.state = 1
		End if 
 '   END If
End Sub

Sub AwardSpecial()
	KnockerSolenoid
    DMD "_", CL(1, ("EXTRA GAME WON")), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("", 122, DOFPulse, DOFKnocker)
	'pupevent missing
    DOF 121, DOFPulse
'    Credits = Credits + 1
	AddCredit 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    Flasheffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot,
    Dim tmp
    DOF 126, DOFPulse
    tmp = INT(RND * 2)
    Select Case tmp
        Case 0:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_jackpot", VolDef
        Case 1:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_jackpot2", VolDef
    End Select
    AddScore Jackpot(CurrentPlayer)
    AddJackpot 150000
    LightEffect 2
    Flasheffect 2
End Sub

Sub AwardDoubleJackpot() 'award a double jackpot
    Dim tmp
    tmp = Jackpot(CurrentPlayer) * 2
    DMD CL(0, FormatScore(tmp)), CL(1, "DOUBLE JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, ""
	PlaySoundVol "vo_doublejackpot", VolDef
    DOF 126, DOFPulse
    AddScore Jackpot(CurrentPlayer) * 2
    AddJackpot 300000
    LightEffect 2
    Flasheffect 2
End Sub

' Not Used 
'Sub AwardSuperJackpot()
'    AddBonusMultiplier 1
'    DMD CL(0, FormatScore(SuperJackpot(CurrentPlayer))), CL(1, "SUPER JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, ""
'	PlaySoundVol "vo_superjackpot", VolDef
'    DOF 126, DOFPulse
'    AddScore SuperJackpot(CurrentPlayer)
'    AddSuperJackpot 250000
'    LightEffect 2
'    Flasheffect 2
'End Sub
'
'Sub AwardSuperJackpot_Bullseye()
'    Dim tmp
'    tmp = SuperJackpot(CurrentPlayer) * BullseyeMultiplier  'Trooper Multiball pupevent 2x 2 events needed 
'    DOF 126, DOFPulse
'    If BullseyeMultiplier> 1 Then
'        DMD CL(0, FormatScore(tmp)), CL(1, "SUPER JACKPOT X" & BullseyeMultiplier), "", eBlinkFast, eNone, eNone, 1500, True, ""
'		PlaySoundVol "vo_superduperjackpot", VolDef
'    Else
'        DMD CL(0, FormatScore(tmp)), CL(1, "SUPER JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, ""
'		PlaySoundVol "vo_superjackpot", VolDef
'    End If
'    AddScore tmp
'    AddSuperJackpot 250000
'    LightEffect 2
'    Flasheffect 2
'End Sub

Sub AwardJackpot_PharaohBullseye() 'target hit
    Dim tmp
    tmp = Jackpot(CurrentPlayer) * BullseyeMultiplier 'Trooper Single Jackpot pupevent hit 3x so 3 events needed
    DOF 126, DOFPulse
    Select Case BullseyeMultiplier
        Case 1:DMD CL(0, FormatScore(tmp)), CL(1, "JACKPOT X" & BullseyeMultiplier), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_jackpot", VolDef
        Case 2:DMD CL(0, FormatScore(tmp)), CL(1, "DOUBLE JACKPOT X" & BullseyeMultiplier), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_doublejackpot", VolDef
        Case 3:DMD CL(0, FormatScore(tmp)), CL(1, "TRIPLE JACKPOT X" & BullseyeMultiplier), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_triplejackpot", VolDef
    End Select
    AddScore tmp
    LightEffect 2
    Flasheffect 2
End Sub

Sub AwardJackpot_Arrow(n) 'Jackpot Arrow hit
    Dim tmp
    tmp = Jackpot(CurrentPlayer) * TrooperMBMultiplier(CurrentPlayer, n) 'Trooper Single Jackpot arrow hit 3x so 3 events needed
    DOF 126, DOFPulse
    Select Case TrooperMBMultiplier(CurrentPlayer, n)
        Case 1:DMD CL(0, FormatScore(tmp)), CL(1, "JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_jackpot", VolDef
        Case 2:DMD CL(0, FormatScore(tmp)), CL(1, "DOUBLE JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_doublejackpot", VolDef
        Case 3:DMD CL(0, FormatScore(tmp)), CL(1, "TRIPLE JACKPOT"), "", eBlinkFast, eNone, eNone, 1500, True, "":PlaySoundVol "vo_triplejackpot", VolDef
        Case Else DMD CL(0, FormatScore(tmp)), CL(1, "JACKPOT X" & TrooperMBMultiplier(CurrentPlayer, n)), "", eBlinkFast, eNone, eNone, 1500, True, "vo_jackpot":PlaySoundVol "vo_jackpot", VolDef
    End Select
    AddScore tmp
    AddJackpot 150000
    LightEffect 2
    Flasheffect 2
End Sub


' Skillshot: https://youtu.be/KFrRH1r8iV4?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=2218
Sub AwardSkillshot()
WriteToLog "     ", "Skillshot"
	ResetSkillShot
    PlaySoundVol "vo_skillshot", VolDef
    DMD CL(0, FormatScore(SkillshotValue(CurrentPlayer))), CL(1, ("SKILLSHOT")), "", eBlinkFast, eNone, eNone, 1500, True, ""

	pClearScore ' ANDREW
	pClearEverything 'ANDREW
	pClearJackpotCounts 'ANDREW
	pClearSplashLoopCount 'ANDREW
	'pupevent 602	
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""SKILL SHOT AWARDED.mp4"", ""SKILL SHOT^^^^^^^" & FormatScore(SkillShotValue(CurrentPlayer)) & "^3.5:+1 EDDIE letter^3.5:+5 seconds Ball Save"", ""^^^^^^^3000:" & pupColorRed & "^^"" ", 3334, 1
	QueueScene "SceneClearLabels", 0, 1

    DOF 127, DOFPulse
    Addscore SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 1 Million
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 1000000

    'do some light show
    GiEffect 2
    LightEffect 2
	TriggerScript 3000, "PupOverlayInGame" 'ANDREW
End Sub


Sub AwardSuperSkillshot()
WriteToLog "     ", "Super Skillshot"
    ResetSkillShot
	PlaySoundVol "vo_superskillshot", VolDef
    DMD CL(0, FormatScore(SuperSkillshotValue(CurrentPlayer))), CL(1, ("SUPER SKILLSHOT")), "", eBlinkFast, eNone, eNone, 1500, True, ""
	pClearScore ' ANDREW
	pClearEverything 'ANDREW
	pClearJackpotCounts 'ANDREW
	pClearSplashLoopCount 'ANDRE
	'pupevent 603
	'QueueScene "pupevent 603", 3000, 1
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""SKILL SHOT AWARDED.mp4"", ""SUPER SKILL SHOT^^^^^^^" & FormatScore(SuperSkillShotValue(CurrentPlayer)) & "^3.5:+1 Playfield Multiplier^3.5:+10 seconds Ball Save"", ""^^^^^^^3000:" & pupColorRed & "^^"" ", 3334, 1
	QueueScene "SceneClearLabels", 0, 1

    DOF 127, DOFPulse
    Addscore SuperSkillShotValue(CurrentPlayer)
    ' increment the superskillshot value with 5 Million
    SuperSkillShotValue(CurrentPlayer) = SuperSkillShotValue(CurrentPlayer) + 5000000
										
	'pupevent 421
	'QueueScene "pupevent 421", 3000, 1		' Award Soul Shard

    'do some light show
    GiEffect 2
    LightEffect 2
    Flasheffect 2
	TriggerScript 3000, "PupOverlayInGame"
End Sub

Sub AwardSuperSecretSkillshot(points)
WriteToLog "     ", "Secret Skillshot"

    ResetSkillShot
	PlaySoundVol "vo_supersecretskillshot", VolDef
    DMD CL(0, FormatScore(points)), CL(1, ("SECRET SKILLSHOT")), "", eBlinkFast, eNone, eNone, 1500, True, ""
	pClearScore ' ANDREW
	pClearEverything 'ANDREW
	pClearJackpotCounts 'ANDREW
	pClearSplashLoopCount 'ANDRE

	'pupevent 604' ANDREW
	QueueScene "pupevent 604", 3370, 1

	DOF 127, DOFPulse
    Addscore points
	'do some light show
    GiEffect 2
    LightEffect 2
    Flasheffect 2
	TriggerScript 3000, "PupOverlayInGame" 'ANDREW
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
	Dim i

' Load Stats
	For i = 0 to 30
		x = LoadValue(TableName, "StatScore" & i)
		If(x <> "") and (x <>"0") Then 
			StatScore(i) = CDbl(x) 
		Else 										' DEFAULT Values 
			Select case i
				case 0: StatScore(i) = 20000000 	' FOI Champ	(flight Of Icarus)
				case 1: StatScore(i) = 3 			' FOI Combo King
				case 2: StatScore(i) = 10000000 	' Hallowed Champ
				case 3: StatScore(i) = 30000	 	' Hallowed Escape Artist
				case 4: StatScore(i) = 15000000 	' Rime Champ
				case 5: StatScore(i) = 20000000 	' FOTD Champ
				case 6: StatScore(i) = 15000000 	' Aces Champ
				case 7: StatScore(i) = 4		 	' Aces Air Ace
				case 8: StatScore(i) = 50000000 	' TM2M Champ
				case 9: StatScore(i) = 25000000 	' TrooperMB Champ
				case 10: StatScore(i) = 15000000 	' MummyMB Champ
				case 11: StatScore(i) = 35000000 	' CyborgMB Champ
				case 12: StatScore(i) = 15000000 	' Madness Champ
				case 13: StatScore(i) = 10000000 	' RTTH Champ
				case 14: StatScore(i) = 50000000 	' NOTB Champ
				case 15: StatScore(i) = 2000000 	' Deathblow Champ
				case 16: StatScore(i) = 15		 	' Loop Champ
				case 17: StatScore(i) = 20000000 	' Loop JP Champ
				case 18: StatScore(i) = 50000000 	' Power JP Champ
				case 19: StatScore(i) = 100		 	' Spinner Master
				case 20: StatScore(i) = 10		 	' Spinner Meltdown Master
				case 21: StatScore(i) = 5		 	' 3 Bank Target Champ
				case 22: StatScore(i) = 3		 	' Pharaoh Target Sharpshooter 
				case 23: StatScore(i) = 20000000 	' Bonus Meltdown Master 
				case 24: StatScore(i) = 25000000 	' PF-X Champ
				case 25: StatScore(i) = 6000000 	' Supersling Champ
				case 26: StatScore(i) = 3		 	' Undead Champ
				case 27: StatScore(i) = 3		 	' ? Master
				case 28: StatScore(i) = 1		 	' Super Skillshot Champ
				case 29: StatScore(i) = 10		 	' Combo Champ
			End Select 

		End If
		x = LoadValue(TableName, "StatName" & i)
		If(x <> "")Then 
			StatName(i) = x 
		Else 
			Select Case i
				case 0:
					StatName(i) = "TMT"		' TOMATE
				case 1:
					StatName(i) = "APO"		' APOPHIS
				case 2:
					StatName(i) = "SIX"		' SIX 
				case 3:
					StatName(i) = "IAK"		
				case 4:
					StatName(i) = "ASS"		' ASTRO
				case 5:
					StatName(i) = "TER"		
				case 6:
					StatName(i) = "CAR"		' Darkstar	
				case 7:
					StatName(i) = "HRZ"
				case 8:
					StatName(i) = "MPT"		' MPT3K
				case 9:
					StatName(i) = "ESD"		' 
				case 15:
					StatName(i) = "WTO"		' Seirif
				case else 
					StatName(i) = "AAA"
			End Select 
		End If
	Next 


    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 250000000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "RIK" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 200000000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "DAP" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 150000000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CMP" End If			' RetroG33K
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "AJT" End If			' SoundScape
    x = LoadValue(cGameName, "HighScore5")
    If(x <> "")then HighScore(4) = CDbl(x)Else HighScore(4) = 50000000 End If
    x = LoadValue(cGameName, "HighScore5Name")
    If(x <> "")then HighScoreName(4) = x Else HighScoreName(4) = "NBS" End If			' Nailbuster

    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
	Dim i
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "HighScore5", HighScore(4)
    SaveValue cGameName, "HighScore5Name", HighScoreName(4)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed

	For i = 0 to 30
		if StatScore(i) <> 0 then 
			SaveValue TableName, "StatScore" & i, StatScore(i)
			SaveValue TableName, "StatName" & i, StatName(i)
		End if 
	Next 

End Sub

Sub ClearAll()
	Clearhs
	SaveValue TableName,  "DMDStd_"&kDMDStd_Initials, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_ExtraBallLimit, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_ExtraBallPCT, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_TiltWarn, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_TiltDebounce, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_MatchPCT, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_LeftStartReset, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_BallSave, ""
	SaveValue TableName,  "DMDStd_"&kDMDStd_BallSaveExtend, ""

	SaveValue TableName,  "DMDFet_"&kDMDFet_FamilyMode, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_MadnessEnabled, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_ReviveStartCnt, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_InactivityPause, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_TimerFearOfTheDark, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_TimerHallowed, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_TimerIcarus, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_TimeAcesBallSave, ""
	SaveValue TableName,  "DMDFet_"&kDMDFet_TimeRimeBallSave, ""
	SaveValue TableName,  "dmdCriticalChanged", ""
	SaveValue TableName,  "ReplayValue", ""

End Sub 

Sub Clearhs
	Dim i
	For i = 0 to 30
		StatScore(i)=0 
		StatName(i)="" 
	Next 

HighScore(0) = 250000000 
HighScoreName(0) = "AAA"
HighScore(1) = 200000000 
HighScoreName(1) = "BBB"
HighScore(2) = 150000000 
HighScoreName(2) = "CCC"
HighScore(3) = 100000000 
HighScoreName(3) = "DDD"
HighScore(4) = 50000000 
HighScoreName(4) = "EEE"
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "HighScore5", HighScore(4)
    SaveValue TableName, "HighScore5Name", HighScoreName(4)
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Dim hsMAXStart:hsMAXStart=20
Dim hsMAX:hsMAX=10	        'V                                                 26                                                  V
Dim hsStrSelect:hsStrSelect="N O P Q R S T U V W X Y Z : ; 0 1 2 3 4 5 6 7 8 9   A B C D E F G H I J K L M N O P Q R S T U V W X Y Z : ; 0 1 2 3 4 5 6 7 8 9   A B C D E F G H I J K L M "
Dim hsCurrentPlace
Dim HSCheckPlayer 
Dim bAchScoreOnly		' No HighScore just Champ for Xandar, Cherry or Immolation

' Notes: Font and layout is incorrect 
' https://www.youtube.com/watch?v=besY8TS0Ges&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&index=7
Sub CheckHighscore()
WriteToLog "     ", "CheckHighscore"

'Score(CurrentPlayer)=250000001
'ModePoints(CurrentPlayer, kModeIcarus)=200000000

   'osbtempscore = Score(CurrentPlayer) 'Andrew
	Dim i
	Dim tmp
	bAchScoreOnly=False
    tmp = Score(CurrentPlayer)
'    If Score(1)> tmp Then tmp = Score(1)
'    If Score(2)> tmp Then tmp = Score(2)
'    If Score(3)> tmp Then tmp = Score(3)
'
    If tmp> HighScore(0)Then 'add 1 credit for beating the highscore
'        Credits = Credits + 1
		AddCredit 1
        DOF 125, DOFOn
    End If

	HSCheckPlayer=CurrentPlayer
	LastScore(HSCheckPlayer)=Score(HSCheckPlayer)	' Save off for attract later

' Debug to Force high score screen
'tmp=10
'HighScore(3)=0

	For i = 0 to 30
		select case i
			case 0:
				if ModePoints(CurrentPlayer, kModeIcarus) > StatScore(i) then 	' FOI Champ	(Flight Of Icarus)
					bAchScoreOnly=True 
				End if 
			case 1:																' Flight Of Icarus Combo King
			case 2: 
				if ModePoints(CurrentPlayer, kModeHallowed) > StatScore(i) then 	' Hallowed Champ
					bAchScoreOnly=True 
				End if 
			case 3: 																' Hallowed Escape Artist
			case 4: 
				if ModePoints(CurrentPlayer, kModeRime) > StatScore(i) then 	' Rime Champ
					bAchScoreOnly=True 
				End if 
			case 5:
				if ModePoints(CurrentPlayer, kModeFear) > StatScore(i) then 	' FOTD Champ
					bAchScoreOnly=True 
				End if 
			case 6:
				if ModePoints(CurrentPlayer, kModeAces) > StatScore(i) then 	' Aces Champ
					bAchScoreOnly=True 
				End if 
			case 7: 															' Aces Air Ace
			case 8:
				if ModePoints(CurrentPlayer, kMode2M2M) > StatScore(i) then 	' TM2M Champ
					bAchScoreOnly=True 
				End if 
			case 9:
				if ModePoints(CurrentPlayer, kModeTrooper) > StatScore(i) then 	' TrooperMB Champ
					bAchScoreOnly=True 
				End if 
			case 10:
				if ModePoints(CurrentPlayer, kModeMummy) > StatScore(i) then 	' MummyMB Champ
					bAchScoreOnly=True 
				End if 
			case 11:
				if ModePoints(CurrentPlayer, kModeCyborg) > StatScore(i) then 	' CyborgMB Champ
					bAchScoreOnly=True 
				End if 
			case 12:
				if ModePoints(CurrentPlayer, kModeMadness) > StatScore(i) then 	' Madness Champ
					bAchScoreOnly=True 
				End if 
			case 13: 
				if ModePoints(CurrentPlayer, kModeRTTH) > StatScore(i) then 	' RTTH Champ
					bAchScoreOnly=True 
				End if 
			case 14:
				if ModePoints(CurrentPlayer, kModeNOTB) > StatScore(i) then 	' NOTB Champ
					bAchScoreOnly=True 
				End if 
			case 15: 	' Deathblow Champ
			case 16: 	' Loop Champ
			case 17: 	' Loop JP Champ
			case 18: 	' Power JP Champ
			case 19: 	' Spinner Master
			case 20: 	' Spinner Meltdown Master
			case 21: 	' 3 Bank Target Champ
			case 22: 	' Pharaoh Target Sharpshooter 
			case 23: 	' Bonus Meltdown Master 
			case 24: 	' PF-X Champ
			case 25: 	' Supersling Champ
			case 26: 	' Undead Champ
			case 27: 	' ? Master
			case 28: 	' Super Skillshot Champ
			case 29:
				if ComboTotal(CurrentPlayer) > StatScore(i) then 	' Combo Champ
					bAchScoreOnly=True 
				End if 
		End Select 
	Next

	if tmp>=HighScore(4) then bAchScoreOnly=False 
    If tmp>=HighScore(4) or bAchScoreOnly Then
        KnockerSolenoid : DOF 122, DOFPulse
        DOF 121, DOFPulse
		if bAchScoreOnly = False then 
			HighScore(4) = tmp
		End if 
		playclear pMusic								' Stop BG Music 
		PlaySoundVolLoop "sfx_highscore", VolSfx		' Play Maiden BG music

        'enter player's name

		pClearEverything
		PupOverlayBonusSc
		QueueFlush 0
		playclear pDMDFull
		PuPlayer.playlistplayex pDMdFull,"PupOverlays","clear1.png", 1, 1
		puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "P3Bg", "PupOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMdFull, "P4Bg", "PupOverlays\\clear.png",1,""
		PuPlayer.LabelShowPage pDMDFull, 3,0,""

		puPlayer.LabelSet pDMDFull,"HSIntro1", "Player " & HSCheckPlayer+1	,1,"{'mt':2,'color':" & pupColorWhite & ", 'size':10}"
		puPlayer.LabelSet pDMDFull,"HSIntro2", "ENTER INITIALS"				,1,"{'mt':2,'color':" & pupColorWhite & ", 'size':10}"
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""Backglass"", ""Cloud.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ",3000, 1
		QueueScene "SceneClearLabels", 0, 1
		QueueScene "HighScoreEntryInit ", 0, 1

    Else
		EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
WriteToLog "     ", "HighScoreEntryInit:"
	Dim i

	SceneGeneralStart pDMDFull, True, False, "Backglass", "HighScoreBG.mp4", "^^^^^^^^^", "^^^^^^^^^"

	hsMAX = DMDStd(kDMDStd_Initials)+1
	if hsMax=4 then hsMAXStart=40			' where do fields start on the screen
	' Reset underscores 
	for i = 1 to 11
		puPlayer.LabelSet pDMDFull,"EnterHS3" & i, " ", 1, "{mt:2,'xpos':" & hsMAXStart+(i*5) & "}"
		if i <= hsMax then 
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & i, "_", 1, "{mt:2,'xpos':" & hsMAXStart+(i*5) & "}"
		else 
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & i, "", 1, "{mt:2,'xpos':" & hsMAXStart+(i*5) & "}"
		End if 
	Next 

	puPlayer.LabelSet pDMDFull,"HSIntro1", " "	,1,""
	puPlayer.LabelSet pDMDFull,"HSIntro2", " "	,1,""
	puPlayer.LabelSet pDMDFull,"EnterHS1", "Player " & HSCheckPlayer+1 & " - Enter Initials"		,1,""


	if Score(HSCheckPlayer) > HighScore(0) then 
		puPlayer.LabelSet pDMDFull,"EnterHS2", "GRAND CHAMPION" ,1,""
		hsCurrentPlace=0
	Else 
		if Score(HSCheckPlayer) > HighScore(1) then 
			hsCurrentPlace=1
			puPlayer.LabelSet pDMDFull,"EnterHS2", "HIGH SCORE #1" ,1,""
		elseif Score(HSCheckPlayer) > HighScore(2) then 
			hsCurrentPlace=2
			puPlayer.LabelSet pDMDFull,"EnterHS2", "HIGH SCORE #2" ,1,""
		elseif Score(HSCheckPlayer) > HighScore(3) then 
			hsCurrentPlace=3
			puPlayer.LabelSet pDMDFull,"EnterHS2", "HIGH SCORE #3" ,1,""
		elseif Score(HSCheckPlayer) > HighScore(4) then 
			hsCurrentPlace=4
			puPlayer.LabelSet pDMDFull,"EnterHS2", "HIGH SCORE #4" ,1,""
		else 
			hsCurrentPlace=4
			puPlayer.LabelSet pDMDFull,"EnterHS2", "HIGH SCORE" ,1,""
		End if 
	End if 
	hsEnteredName=""

	hsbModeActive = True
    PlaySoundVol "vo_highscore", VolDef
    hsLetterFlash = 0

    hsCurrentDigit = 1

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ:;0123456789"    ' < is used to delete the last letter
    hsCurrentLetter = 2
    DMDFlush()
    HighScoreDisplayName()

    HighScoreFlashTimer.Interval = 100
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreFlashTimer_Timer()
'    if(hsCurrentDigit <> hsMAX) then
        if(hsLetterFlash mod 2 =0) then
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & hsCurrentDigit, " " ,1,""
        else
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & hsCurrentDigit, "_" ,1,""
        end if

		if(hsLetterFlash=0) then
			puPlayer.LabelSet pDMDFull,"EnterHS4Sel", "", 1, ""
        elseif(hsLetterFlash=3) then
			puPlayer.LabelSet pDMDFull,"EnterHS4Sel", "[ ]", 1, "{'mt':2,'xpos':48.7}"
        end if

 '   end if
	hsLetterFlash=hsLetterFlash+1
	if hsLetterFlash=6 then hsLetterFlash=0
End Sub 

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey and (hsCurrentDigit <> hsMAX or hsCurrentLetter=29) Then
        PlaySoundVol "sfx-hs-left", VolSfx
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey  and (hsCurrentDigit <> hsMAX or hsCurrentLetter = 28) Then
        PlaySoundVol "sfx-hs-right", VolSfx
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

	' Stern key - same as start key 
	If (keycode = RightMagnaSave and bDebugLeftMagnaDown=False) or keycode = LockBarKey or _  
			(keycode = PlungerKey and bUsePlungerForSternKey) Then
' just does the same as the start key 
'		hsCurrentLetter = 29
'		HighScoreDisplayName()
		keycode = StartGameKey
	End if 

    If keycode = StartGameKey Then
		if(mid(hsValidLetters, hsCurrentLetter, 1) = ";")then		' Commit 
			HighScoreCommitName()
        elseif(mid(hsValidLetters, hsCurrentLetter, 1) <> ":")then		' Not Delete
			if hsCurrentDigit <> hsMAX then 
				PlaySoundVol "sfx-hs-select", VolSfx
				
				HighScoreFlashTimer.Enabled=False 
				hsEnteredName= hsEnteredName&mid(hsValidLetters, hsCurrentLetter, 1)
				puPlayer.LabelSet pDMDFull,"EnterHSU3" & hsCurrentDigit, " " ,1,""
				hsCurrentDigit = hsCurrentDigit + 1
				HighScoreFlashTimer.Enabled=True

				if(hsCurrentDigit = hsMAX)then
					hsCurrentLetter = 29
					HighScoreDisplayName()
	'                HighScoreCommitName()
				else
					HighScoreDisplayName()
				end if
			End if 
        else
			'PlaySoundVol "fx_Esc", VolDef
			
			HighScoreFlashTimer.Enabled=False 
			puPlayer.LabelSet pDMDFull,"EnterHS3" & hsCurrentDigit, "" ,1,""
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & hsCurrentDigit, "_" ,1,""
            if(hsCurrentDigit > 1)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
			HighScoreFlashTimer.Enabled=True
			hsEnteredName= mid(hsEnteredName, 1, hsCurrentDigit-1)
            HighScoreDisplayName()
        end if
'WriteToLog "     ", "NAME: " & hsEnteredName
    end if
End Sub

Sub HighScoreDisplayName()
Dim i, TempStr
	i = (32+hsCurrentLetter)*2
'WriteToLog "     ", "i=" & i
	puPlayer.LabelSet pDMDFull,"EnterHS4", Mid(hsStrSelect, i-(16*2), (26*2)), 1,"{'mt':2,'xpos':2.8}"
	puPlayer.LabelSet pDMDFull,"EnterHS3" & hsCurrentDigit, mid(hsValidLetters, hsCurrentLetter, 1) ,1,""
	puPlayer.LabelSet pDMDFull,"EnterHS3Glyph" & hsCurrentDigit, mid(hsValidLetters, hsCurrentLetter, 1) ,1,""
End Sub


Sub HighScoreCommitName()
	Dim i
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    if(hsEnteredName = "")then
        hsEnteredName = "YOU"
    end if

	For i = 0 to 30
		select case i
			case 0:
				if ModePoints(CurrentPlayer, kModeIcarus) > StatScore(i) then 	' FOI Champ	(flight Of Icarus)
					StatScore(i)=ModePoints(CurrentPlayer, kModeIcarus)
					StatName(i)=hsEnteredName
				End if 
			case 1:
				if ComboCompletions(CurrentPlayer) > StatScore(i) then 	' FOI Combo King
					StatScore(i)=ComboCompletions(CurrentPlayer)
					StatName(i)=hsEnteredName
				End if 
			case 2: 
				if ModePoints(CurrentPlayer, kModeHallowed) > StatScore(i) then 	' Hallowed Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeHallowed)
					StatName(i)=hsEnteredName
				End if 
			case 3: 																' Hallowed Escape Artist
			case 4: 
				if ModePoints(CurrentPlayer, kModeRime) > StatScore(i) then 	' Rime Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeRime)
					StatName(i)=hsEnteredName
				End if 
			case 5:
				if ModePoints(CurrentPlayer, kModeFear) > StatScore(i) then 	' FOTD Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeFear)
					StatName(i)=hsEnteredName
				End if 
			case 6:
				if ModePoints(CurrentPlayer, kModeAces) > StatScore(i) then 	' Aces Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeAces)
					StatName(i)=hsEnteredName
				End if 
			case 7: 															' Aces Air Ace
			case 8:
				if ModePoints(CurrentPlayer, kMode2M2M) > StatScore(i) then 	' TM2M Champ
					StatScore(i)=ModePoints(CurrentPlayer, kMode2M2M)
					StatName(i)=hsEnteredName
				End if 
			case 9:
				if ModePoints(CurrentPlayer, kModeTrooper) > StatScore(i) then 	' TrooperMB Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeTrooper)
					StatName(i)=hsEnteredName
				End if 
			case 10:
				if ModePoints(CurrentPlayer, kModeMummy) > StatScore(i) then 	' MummyMB Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeMummy)
					StatName(i)=hsEnteredName
				End if 
			case 11:
				if ModePoints(CurrentPlayer, kModeCyborg) > StatScore(i) then 	' CyborgMB Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeCyborg)
					StatName(i)=hsEnteredName
				End if 
			case 12:
				if ModePoints(CurrentPlayer, kModeMadness) > StatScore(i) then 	' Madness Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeMadness)
					StatName(i)=hsEnteredName
				End if 
			case 13: 
				if ModePoints(CurrentPlayer, kModeRTTH) > StatScore(i) then 	' RTTH Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeRTTH)
					StatName(i)=hsEnteredName
				End if 
			case 14:
				if ModePoints(CurrentPlayer, kModeNOTB) > StatScore(i) then 	' NOTB Champ
					StatScore(i)=ModePoints(CurrentPlayer, kModeNOTB)
					StatName(i)=hsEnteredName
				End if 
			case 15: 	' Deathblow Champ
			case 16: 	' Loop Champ
			case 17: 	' Loop JP Champ
			case 18: 	' Power JP Champ
			case 19: 	' Spinner Master
			case 20: 	' Spinner Meltdown Master
			case 21: 	' 3 Bank Target Champ
			case 22: 	' Pharaoh Target Sharpshooter 
			case 23: 	' Bonus Meltdown Master 
			case 24: 	' PF-X Champ
			case 25: 	' Supersling Champ
			case 26: 	' Undead Champ
			case 27: 	' ? Master
			case 28: 	' Super Skillshot Champ
			case 29:
				if ComboTotal(CurrentPlayer) > StatScore(i) then 	' Combo Champ
					StatScore(i)=ComboTotal(CurrentPlayer)
					StatName(i)=hsEnteredName
				End if 
		End Select 
	Next


	if bAchScoreOnly=False then 
		HighScoreName(4) = hsEnteredName
		SortHighscore
	End if 

' Clear PupDMD screen once high score entry is commited
	puPlayer.LabelSet pDMDFull,"Congrats", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHSPlayer", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHSInitials", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHS1", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHS2", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHS4", "",1,""
	puPlayer.LabelSet pDMDFull,"EnterHS4Sel", "", 1, ""
	puPlayer.LabelSet pDMDFull,"FinalScore","",1,""
	for i = 1 to 11
		if i <= 3 then puPlayer.LabelSet pDMDFull,"EnterHS3Glyph" & i, " " ,1,""
		puPlayer.LabelSet pDMDFull,"EnterHS3" & i, " ", 1, ""
		puPlayer.LabelSet pDMDFull,"EnterHSU3" & i, " ", 1, ""
	Next 

' Submit to Orbital
	'osbtemp = hsEnteredName 'Andrew
	'SubmitOSBScore 'Andrew

	StopSound "sfx_highscore"
	PuPlayer.LabelShowPage pDMDFull, 1, 0, ""
	EndOfBallComplete()

End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 4
        For j = 0 to 3
            If HighScore(j) <HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage, myPrevLUT ' Andrew added myPrevLUT dim
Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0

	if LUTimage = 9 then LUTimage = 0			' Fix Bug of dark LUT
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub myNextLUT
	LUTImage = (LUTImage + 1)MOD 10
	UpdateLUT
	SaveLUT
	myPrevLUT = LUTImage ' Andrew Added
End Sub

'Andrew added 2 new subs below
Sub RecallPrevLut
	LUTImage = myPrevLUT
	UpdateLUT
	SaveLUT
End Sub

Sub DarkLut
	LUTimage = 9
	UpdateLUT
	SaveLUT
End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
    End Select
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values - note FLEXDMD can only handle 20 characters
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            FlexDMD.TableFile = Table1.Filename & ".vpx"
            FlexDMD.RenderMode = 2
            FlexDMD.Width = 128
            FlexDMD.Height = 32
            FlexDMD.Clear = True
            FlexDMD.GameName = cGameName
            FlexDMD.Run = True
            Set DMDScene = FlexDMD.NewGroup("Scene")
            DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.DMD_blank")
            DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
            For i = 0 to 40
                DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                Digits(i).Visible = False
            Next
            digitgrid.Visible = False
            For i = 0 to 19 ' Top
                DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 7, 10
            Next
            For i = 20 to 39 ' Bottom
                DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 3 + 12 + 2, 7, 10
            Next
            FlexDMD.LockRenderThread
            FlexDMD.Stage.AddActor DMDScene
            FlexDMD.UnlockRenderThread
        End If
    End If

    Dim i, j
    DMDFlush()
    deSpeed = 30
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 20 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = FL(0, "PL " &CurrentPlayer+1, FormatScore(Score(Currentplayer)))
        tmp1 = CL(1, "CREDITS " & Credits & " BALL " & Balls)
        If bskillshotready Then
            'tmp1 = " HIT THE SKILLSHOT"
			'pupDMDDisplay "skillshot", "HIT THE SKILLSHOT|RGB(255,255,255)", "", 5, 1, 10
        ElseIf LoopCount> 0 Then
            tmp1 = CL(1, "SHOOT THE LOOP")
        ElseIf lBattle.State then
            tmp1 = CL(1, "SHOOT THE UNDERWORLD")
        ElseIf lLock.State then
            tmp1 = CL(1, "LOCK THE BALL")
        ElseIf bTombTreasureReady(CurrentPlayer) Then
            tmp1 = "TOMB TREASURE IS LIT"
        ELSE
            Select Case Mode(CurrentPlayer, 0)
                Case 1:tmp1 = "MUMMY MULTIBALL" ' 2-Ball Multiball, collect 7 jackpots, complete yellow shots for add-a-balls
                Case 2:tmp1 = "TROOPER MULTIBALL" ' drop targets light, lock 3 balls to begin multiball. shoot lit arrows to score jackpot, use upper flippers to shoot loops
                Case 3:tmp1 = "CYBORG MULTIBALL" '3-Ball Multiball
                Case 4:tmp1 = "FLIGHT OF ICARUS"  ' 40 second ramp combo - shoot alternating ramps
                Case 5:tmp1 = "FEAR OF THE DARK" '4 purple arrows will light then spinners light
                Case 6:tmp1 = "ACES HIGH" ' 2 ball multiball - 3 stages (shoot blue arrows, shoot ramp, shoot ORBLight)
                Case 7:tmp1 = "HALLOWED BE THY NAME" ' shoot orange arrows, shoot drop targets, shoot centre ramp, you get 40 seconds.
                Case 8:tmp1 = "RIME ANCIENT MARINER" ' Centre ramp shot lit to collect hurry-up, shoot lit sides during multiball, shoot under centre ramp
                Case 9:tmp1 = "2 MINUTES TO MIDNIGHT" ' Complete all 5 Eddie Battle Modes, complete each lit stand-up target, collect all to light Super Jackpot
                Case 10:tmp1 = "SHOOT WHITE ARROWS" 'to spell E-D-D-I-E 
                Case 11:tmp1 = "CAN I PLAY W/MADNESS" 'Shoot all red lit arrows to Green, complete for Madness Jackpot
                Case 12:tmp1 = "RUN TO THE HILLS" '6-BALL MULTIBALL
            End Select
            tmp2 = ""
        End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_")Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1)Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySoundVol dqSound(dqHead), VolDef
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i)- 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1)- deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i)- 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) <dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

	if Num=0 then 
		FormatScore="00"
		Exit Function
	End if 

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)   ' ANDREW
		   'NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(id, NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString1)- Len(NumString2)
	if Temp>=0 then 
		TempStr = NumString1 & Space(Temp) & NumString2
	else 
		TempStr = NumString1 & NumString2
	End if 

    FL = TempStr
End Function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
	if Temp>=0 then 
		TempStr = Space(Temp) & NumString & Space(Temp)
	else 
		TempStr = NumString
	End if 

    CL = TempStr
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString)
	if Temp>=0 then 
		TempStr = Space(Temp) & NumString
	else 
		TempStr = NumString
	End if 
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "DMD_blank"
            Digits(40).ImageA = dLine(2)
' DAP - Crashes on my CAB 
'            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "dempty":Next

    Chars(43) = "dplus"   '+
    Chars(46) = "ddot"    '.
    Chars(48) = "d0"      '0
    Chars(49) = "d1"      '1
    Chars(50) = "d2"      '2
    Chars(51) = "d3"      '3
    Chars(52) = "d4"      '4
    Chars(53) = "d5"      '5
    Chars(54) = "d6"      '6
    Chars(55) = "d7"      '7
    Chars(56) = "d8"      '8
    Chars(57) = "d9"      '9
    Chars(60) = "dless"   '<
    Chars(61) = "dequal"  '=
    Chars(62) = "dmore"   '>
    Chars(64) = "DMD_blank" '@
    Chars(65) = "da"      'A
    Chars(66) = "db"      'B
    Chars(67) = "dc"      'C
    Chars(68) = "dd"      'D
    Chars(69) = "de"      'E
    Chars(70) = "df"      'F
    Chars(71) = "dg"      'G
    Chars(72) = "dh"      'H
    Chars(73) = "di"      'I
    Chars(74) = "dj"      'J
    Chars(75) = "dk"      'K
    Chars(76) = "dl"      'L
    Chars(77) = "dm"      'M
    Chars(78) = "dn"      'N
    Chars(79) = "do"      'O
    Chars(80) = "dp"      'P
    Chars(81) = "dq"      'Q
    Chars(82) = "dr"      'R
    Chars(83) = "ds"      'S
    Chars(84) = "dt"      'T
    Chars(85) = "du"      'U
    Chars(86) = "dv"      'V
    Chars(87) = "dw"      'W
    Chars(88) = "dx"      'X
    Chars(89) = "dy"      'Y
    Chars(90) = "dz"      'Z
    Chars(94) = "dup"     '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1. 'a
    Chars(98) = "d2a"  '2. 'b
    Chars(99) = "d3a"  '3. 'c
    Chars(100) = "d4a" '4. 'd
    Chars(101) = "d5a" '5. 'e
    Chars(102) = "d6a" '6. 'f
    Chars(103) = "d7a" '7. 'g
    Chars(104) = "d8a" '8. 'h
    Chars(105) = "d9a" '9  'i
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate					'update rolling sounds
	DoDTAnim 						'handle drop target animations
'    ramp007.heightbottom = UnderworldFlipper.currentangle
	primRamp007.rotx=90+UnderworldFlipper.currentangle
	primFlipperR.roty=-RightFlipper.StartAngle+RightFlipper.currentangle
	primFlipperL.roty=-LeftFlipper.StartAngle+LeftFlipper.currentangle
	primFlipperROff.roty=-RightFlipper.StartAngle+RightFlipper.currentangle
	primFlipperLOff.roty=-LeftFlipper.StartAngle+LeftFlipper.currentangle

	PrimRightFlipper1.RotY=-RightFlipper1.StartAngle+RightFlipper1.CurrentAngle
	PrimLeftFlipper1.RotY=-LeftFlipper1.StartAngle+LeftFlipper1.CurrentAngle
	PrimRightFlipperOff.RotY=-RightFlipper1.StartAngle+RightFlipper1.CurrentAngle
	PrimLeftFlipperOff.RotY=-LeftFlipper1.StartAngle+LeftFlipper1.CurrentAngle

	PrimSpinner001.RotX=-Spinner001.currentangle
	PrimSpinner002.RotX=-Spinner002.currentangle

' add any other real time update subs, like gates or diverters, flippers
End Sub



' ***************************************************************************************************
' ******************************  Start FlupperDomes2.2 (Flashers)

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = .3		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim objSpeed(20), ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
Dim bFlasher1Enabled:bFlasher1Enabled=False
InitFlasher 1, "blue"
RotateFlasher 1, -25

Dim bFlasher2Enabled:bFlasher2Enabled=False
InitFlasher 2, "white"
RotateFlasher 2, 270

Dim bFlasher3Enabled:bFlasher3Enabled=False
InitFlasher 3, "white"
RotateFlasher 3, 90

'Dim bFlasher4Enabled:bFlasher4Enabled=False
'InitFlasher 4, "white"
'RotateFlasher 4, 90
Dim bFlasher4Enabled:bFlasher4Enabled=False
InitFlasher 4, "white"
RotateFlasher 4, 0


Dim bFlasher5Enabled:bFlasher5Enabled=False
InitFlasher 5, "red"
RotateFlasher 5, 90


' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 0  ' 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	objSpeed(nr)=2
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(4,120,255) ': objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "orange" : objlight(nr).color = RGB(230,115, 0) : objflasher(nr).color = RGB(255,140,26)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher1(nr)	' Flash once (Maybe there is a better way)
'WriteToLog "     ", "FLASHFLASHER  1  !!!!! " & nr
	ObjLevel(nr) = 1
	FlashFlasher(nr)
End Sub 

Sub FlashFlasher(nr)
'WriteToLog "     ", "FLASHFLASHER!!!! " & nr
 	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
	If nr=3 Then DOF 150, Dofpulse End If	
	If nr=1 Then DOF 152, DOFpulse End If
End Sub

'ComboFlashTimer.Enabled=True 
' ObjLevel(1) = 1 : FlasherFlash1_Timer
Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub
'Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
'Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
'Sub tmrCraneDomeFlash_Timer
'	ObjLevel(9) = 1 : FlasherFlash9_Timer
'End Sub  

tmrFlashers.UserValue=0
tmrFlashers.Interval=200
tmrFlashers.Enabled=True
Sub tmrFlashers_Timer
	tmrFlashers.UserValue=tmrFlashers.UserValue+1
	if tmrFlashers.UserValue=6 then tmrFlashers.UserValue=0
	if bFlasher1Enabled and tmrFlashers.UserValue mod objSpeed(1)=0 then 			' Adjust Speed 
		ObjLevel(1) = 1 : FlasherFlash1_Timer
	End if 
	if bFlasher2Enabled and tmrFlashers.UserValue mod objSpeed(2)=0  then 
		ObjLevel(2) = 1 : FlasherFlash2_Timer
	End if 
	if bFlasher3Enabled and tmrFlashers.UserValue mod objSpeed(3)=0  then 
		ObjLevel(3) = 1 : FlasherFlash3_Timer
	End if 
	if bFlasher4Enabled and tmrFlashers.UserValue mod objSpeed(4)=0  then 
		ObjLevel(4) = 1 : FlasherFlash4_Timer
	End if 
	if bFlasher5Enabled and tmrFlashers.UserValue mod objSpeed(5)=0  then 
		ObjLevel(5) = 1 : FlasherFlash5_Timer
	End if 
End Sub


Sub FlashFlasherSetSpeed(nr, speed)    ' 0=slow, 1=Normal, 2=Fast
	if speed=0 then 
		objSpeed(nr)=7
		objflasher(nr).TimerInterval=80
	elseif speed=1 then 
		objSpeed(nr)=2
		objflasher(nr).TimerInterval=60
	else 
		objSpeed(nr)=1
		objflasher(nr).TimerInterval=12
	End if 
End Sub 


' ******************************  End Flupper Flashers
' ***************************************************************************************************


'******************************************************
'****  LAMPZ by nFozzy
'******************************************************


dim lastgametime
Dim MaterialRedArray: MaterialRedArray = Array("BulbRedOff", "BulbRedOff","BulbRedOff","BulbRedOn")
Dim MaterialYellowArray: MaterialYellowArray = Array("BulbYellowOff", "BulbYellowOff","BulbYellowOff","BulbYellowOn")
Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 16
LampTimer.Enabled = 1

Sub LampTimer1_Timer()

	' Stutter Check
	If gametime-lastgametime > 50 Then WriteToLog "     ", "DROPPING FRAMES!!!!:" & gametime-lastgametime
	lastgametime=gametime 

	dim idx : for idx = 0 to uBound(Lampz.Obj)
		if Lampz.IsLight(idx) then 
			if IsArray(Lampz.obj(idx)) then
				dim tmp : tmp = Lampz.obj(idx)
				Lampz.state(idx) = tmp(0).GetInPlayStateBool
				'WriteToLog "     ", tmp(0).name & " " &  tmp(0).GetInPlayStateBool & " " & tmp(0).IntensityScale  & vbnewline
			Else
				Lampz.state(idx) = Lampz.obj(idx).GetInPlayStateBool
				'WriteToLog "     ", Lampz.obj(idx).name & " " &  Lampz.obj(idx).GetInPlayStateBool & " " & Lampz.obj(idx).IntensityScale  & vbnewline
			end if
		end if
	Next
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	if ticksFromLastEject<1000 then ticksFromLastEject=ticksFromLastEject+1
'WriteToLog "     ", "ticksFromLastEject:" & ticksFromLastEject

	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'WriteToLog "     ", pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'WriteToLog "     ", pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub



Sub InitLampsNF()

	'Make all control lights invisible
	dim c : for each c in aLights : c.visible=false : next

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is the way to do assignments. It'll create arrays automatically / append objects to existing arrays
	'If not using a ROM, then the first light in the object array should be an invisible control light (in this example they
    'are named starting with "lc"). Then, lights should always be controlled via these control lights, including
    'when using Light Sequencers.
	
	Lampz.MassAssign(1)= LightShootAgain
	Lampz.MassAssign(1)= vLightShootAgain
	Lampz.MassAssign(1)= bLightShootAgain
	Lampz.Callback(1) = "DisableLighting pLightShootAgain, 300,"
	
	Lampz.MassAssign(2)= lModeIcarus
	Lampz.MassAssign(2)= vlModeIcarus
	Lampz.MassAssign(2)= blModeIcarus
	Lampz.Callback(2) = "DisableLighting plModeIcarus, 300,"
	
	Lampz.MassAssign(3)= lModeHallowed
	Lampz.MassAssign(3)= vlModeHallowed
	Lampz.MassAssign(3)= blModeHallowed
	Lampz.Callback(3) = "DisableLighting plModeHallowed, 300,"
	
	Lampz.MassAssign(4)= lModeRime
	Lampz.MassAssign(4)= vlModeRime
	Lampz.MassAssign(4)= blModeRime
	Lampz.Callback(4) = "DisableLighting plModeRime, 300,"
	
	Lampz.MassAssign(5)= lModeFear
	Lampz.MassAssign(5)= vlModeFear
	Lampz.MassAssign(5)= blModeFear
	Lampz.Callback(5) = "DisableLighting plModeFear, 300,"
	
	Lampz.MassAssign(6)= lModeAces
	Lampz.MassAssign(6)= vlModeAces
	Lampz.MassAssign(6)= blModeAces
	Lampz.Callback(6) = "DisableLighting plModeAces, 300,"
	
	Lampz.MassAssign(7)= lMode2M2M
	Lampz.MassAssign(7)= vlMode2M2M
	Lampz.MassAssign(7)= blMode2M2M
	Lampz.Callback(7) = "DisableLighting plMode2M2M, 300,"
	
	Lampz.MassAssign(8)= Light004
	Lampz.MassAssign(8)= vLight004
	Lampz.MassAssign(8)= bLight004
	Lampz.Callback(8) = "DisableLighting pLight004, 300,"
	
	Lampz.MassAssign(9)= Light003
	Lampz.MassAssign(9)= vLight003
	Lampz.MassAssign(9)= bLight003
	Lampz.Callback(9) = "DisableLighting pLight003, 300,"
	
	Lampz.MassAssign(10)= Light001
	Lampz.MassAssign(10)= vLight001
	Lampz.MassAssign(10)= bLight001
	Lampz.Callback(10) = "DisableLighting pLight001, 300,"
	
	Lampz.MassAssign(11)= Light022
	Lampz.MassAssign(11)= vLight022
	Lampz.MassAssign(11)= bLight022
	Lampz.Callback(11) = "DisableLighting pLight022, 300,"
	
	Lampz.MassAssign(12)= Light002
	Lampz.MassAssign(12)= vLight002
	Lampz.MassAssign(12)= bLight002
	Lampz.Callback(12) = "DisableLighting pLight002, 300,"
	
	Lampz.MassAssign(13)= lPowerPops
	Lampz.MassAssign(13)= vlPowerPops
	Lampz.MassAssign(13)= blPowerPops
	Lampz.Callback(13) = "DisableLighting plPowerPops, 300,"
	
	Lampz.MassAssign(14)= lPowerTargets
	Lampz.MassAssign(14)= vlPowerTargets
	Lampz.MassAssign(14)= blPowerTargets
	Lampz.Callback(14) = "DisableLighting plPowerTargets, 300,"
	
	Lampz.MassAssign(15)= lPowerSpinner
	Lampz.MassAssign(15)= vlPowerSpinner
	Lampz.MassAssign(15)= blPowerSpinner
	Lampz.Callback(15) = "DisableLighting plPowerSpinner, 300,"
	
	Lampz.MassAssign(16)= lPowerOrbits
	Lampz.MassAssign(16)= vlPowerOrbits
	Lampz.MassAssign(16)= blPowerOrbits
	Lampz.Callback(16) = "DisableLighting plPowerOrbits, 300,"
	
	Lampz.MassAssign(17)= lPowerRamps
	Lampz.MassAssign(17)= vlPowerRamps
	Lampz.MassAssign(17)= blPowerRamps
	Lampz.Callback(17) = "DisableLighting plPowerRamps, 300,"
	
	Lampz.MassAssign(18)= Light014
	Lampz.MassAssign(18)= vLight014
	Lampz.MassAssign(18)= bLight014
	Lampz.Callback(18) = "DisableLighting pLight014, 300,"
	
	Lampz.MassAssign(19)= Light013
	Lampz.MassAssign(19)= vLight013
	Lampz.MassAssign(19)= bLight013
	Lampz.Callback(19) = "DisableLighting pLight013, 300,"
	
	Lampz.MassAssign(20)= Light012
	Lampz.MassAssign(20)= vLight012
	Lampz.MassAssign(20)= bLight012
	Lampz.Callback(20) = "DisableLighting pLight012, 300,"
	
	Lampz.MassAssign(21)= Light007
	Lampz.MassAssign(21)= vLight007
	Lampz.MassAssign(21)= bLight007
	Lampz.Callback(21) = "DisableLighting pLight007, 300,"
	
	Lampz.MassAssign(22)= Light006
	Lampz.MassAssign(22)= vLight006
	Lampz.MassAssign(22)= bLight006
	Lampz.Callback(22) = "DisableLighting pLight006, 300,"
	
	Lampz.MassAssign(23)= Light072
	Lampz.MassAssign(23)= vLight072
	Lampz.MassAssign(23)= bLight072
	Lampz.Callback(23) = "DisableLighting pLight072, 300,"
	
	Lampz.MassAssign(24)= Light073
	Lampz.MassAssign(24)= vLight073
	Lampz.MassAssign(24)= bLight073
	Lampz.Callback(24) = "DisableLighting pLight073, 300,"
	
	Lampz.MassAssign(25)= lNOTB
	Lampz.MassAssign(25)= vlNOTB
	Lampz.MassAssign(25)= blNOTB
	Lampz.Callback(25) = "DisableLighting plNOTB, 300,"
	
	Lampz.MassAssign(26)= lCardMummy
	Lampz.MassAssign(26)= vlCardMummy
	Lampz.MassAssign(26)= blCardMummy
	Lampz.Callback(26) = "DisableLighting plCardMummy, 300,"
	
	Lampz.MassAssign(27)= lCardCyborg
	Lampz.MassAssign(27)= vlCardCyborg
	Lampz.MassAssign(27)= blCardCyborg
	Lampz.Callback(27) = "DisableLighting plCardCyborg, 300,"
	
	Lampz.MassAssign(28)= lCardTrooper
	Lampz.MassAssign(28)= vlCardTrooper
	Lampz.MassAssign(28)= blCardTrooper
	Lampz.Callback(28) = "DisableLighting plCardTrooper, 300,"
	
	Lampz.MassAssign(29)= lCard2M2M
	Lampz.MassAssign(29)= vlCard2M2M
	Lampz.MassAssign(29)= blCard2M2M
	Lampz.Callback(29) = "DisableLighting plCard2M2M, 300,"
	
	Lampz.MassAssign(30)= Light005
	Lampz.MassAssign(30)= vLight005
	Lampz.MassAssign(30)= bLight005
	Lampz.Callback(30) = "DisableLighting pLight005, 300,"
	
	Lampz.MassAssign(31)= lPowerSpinnerArrow1
	Lampz.MassAssign(31)= vlPowerSpinnerArrow1
	Lampz.MassAssign(31)= blPowerSpinnerArrow1
	Lampz.Callback(31) = "DisableLighting plPowerSpinnerArrow1, 300,"
	
	Lampz.MassAssign(32)= lSpinnerLeft
	Lampz.MassAssign(32)= vlSpinnerLeft
	Lampz.MassAssign(32)= blSpinnerLeft
	Lampz.Callback(32) = "DisableLighting plSpinnerLeft, 380,"
	
	Lampz.MassAssign(33)= lOrbArrow
	Lampz.MassAssign(33)= vlOrbArrow
	Lampz.MassAssign(33)= blOrbArrow
	Lampz.Callback(33) = "DisableLighting plOrbArrow, 380,"
	
	Lampz.MassAssign(34)= Light045
	Lampz.MassAssign(34)= vLight045
	Lampz.MassAssign(34)= bLight045
	Lampz.Callback(34) = "DisableLighting pLight045, 380,"
	
	Lampz.MassAssign(35)= ORBLight
	Lampz.MassAssign(35)= vORBLight
	
	Lampz.MassAssign(36)= lRampArrow
	Lampz.MassAssign(36)= vlRampArrow
	Lampz.MassAssign(36)= blRampArrow
	Lampz.Callback(36) = "DisableLighting plRampArrow, 300,"
	
	Lampz.MassAssign(37)= lRampRight
	Lampz.MassAssign(37)= vlRampRight
	Lampz.MassAssign(37)= blRampRight
	Lampz.Callback(37) = "DisableLighting plRampRight, 380,"
	
	Lampz.MassAssign(38)= lOrbitArrow2
	Lampz.MassAssign(38)= vlOrbitArrow2
	Lampz.MassAssign(38)= blOrbitArrow2
	Lampz.Callback(38) = "DisableLighting plOrbitArrow2, 300,"
	
	Lampz.MassAssign(39)= lOrbitRight
	Lampz.MassAssign(39)= vlOrbitRight
	Lampz.MassAssign(39)= blOrbitRight
	Lampz.Callback(39) = "DisableLighting plOrbitRight, 380,"
	
	Lampz.MassAssign(40)= lExtraBall
	Lampz.MassAssign(40)= vlExtraBall
	Lampz.MassAssign(40)= blExtraBall
	Lampz.Callback(40) = "DisableLighting plExtraBall, 380,"
	
	Lampz.MassAssign(41)= Light048
	Lampz.MassAssign(41)= vLight048
	Lampz.MassAssign(41)= bLight048
	Lampz.Callback(41) = "DisableLighting pLight048, 380,"
	
	Lampz.MassAssign(42)= lRampLeft
	Lampz.MassAssign(42)= vlRampLeft
	Lampz.MassAssign(42)= blRampLeft
	Lampz.Callback(42) = "DisableLighting plRampLeft, 380,"
	
	Lampz.MassAssign(43)= lLoopLeft
	Lampz.MassAssign(43)= vlLoopLeft
	Lampz.MassAssign(43)= blLoopLeft
	Lampz.Callback(43) = "DisableLighting plLoopLeft, 380,"
	
	Lampz.MassAssign(44)= lPowerSpinnerArrow2
	Lampz.MassAssign(44)= vlPowerSpinnerArrow2
	Lampz.MassAssign(44)= blPowerSpinnerArrow2
	Lampz.Callback(44) = "DisableLighting plPowerSpinnerArrow2, 300,"
	
	Lampz.MassAssign(45)= lOrbitArrow
	Lampz.MassAssign(45)= vlOrbitArrow
	Lampz.MassAssign(45)= blOrbitArrow
	Lampz.Callback(45) = "DisableLighting plOrbitArrow, 300,"
	
	Lampz.MassAssign(46)= lOrbitLeft
	Lampz.MassAssign(46)= vlOrbitLeft
	Lampz.MassAssign(46)= blOrbitLeft
	Lampz.Callback(46) = "DisableLighting plOrbitLeft, 380,"
	
	Lampz.MassAssign(47)= lCaptiveBall
	Lampz.MassAssign(47)= vlCaptiveBall
	Lampz.MassAssign(47)= blCaptiveBall
	Lampz.Callback(47) = "DisableLighting plCaptiveBall, 300,"
	
	Lampz.MassAssign(48)= lMummyM
	Lampz.MassAssign(48)= vlMummyM
	Lampz.MassAssign(48)= blMummyM
	Lampz.Callback(48) = "DisableLighting plMummyM, 300,"
	
	Lampz.MassAssign(49)= lMummyU
	Lampz.MassAssign(49)= vlMummyU
	Lampz.MassAssign(49)= blMummyU
	Lampz.Callback(49) = "DisableLighting plMummyU, 300,"
	
	Lampz.MassAssign(50)= lMummyM2
	Lampz.MassAssign(50)= vlMummyM2
	Lampz.MassAssign(50)= blMummyM2
	Lampz.Callback(50) = "DisableLighting plMummyM2, 300,"
	
	Lampz.MassAssign(51)= lMummyM3
	Lampz.MassAssign(51)= vlMummyM3
	Lampz.MassAssign(51)= blMummyM3
	Lampz.Callback(51) = "DisableLighting plMummyM3, 300,"
	
	Lampz.MassAssign(52)= lMummyY
	Lampz.MassAssign(52)= vlMummyY
	Lampz.MassAssign(52)= blMummyY
	Lampz.Callback(52) = "DisableLighting plMummyY, 300,"
	
	Lampz.MassAssign(53)= lBonusX
	Lampz.MassAssign(53)= vlBonusX
	Lampz.MassAssign(53)= blBonusX
	Lampz.Callback(53) = "DisableLighting plBonusX, 300,"
	
	Lampz.MassAssign(54)= lLightOrb
	Lampz.MassAssign(54)= vlLightOrb
	Lampz.MassAssign(54)= blLightOrb
	Lampz.Callback(54) = "DisableLighting plLightOrb, 300,"
	
	Lampz.MassAssign(55)= lLightLock
	Lampz.MassAssign(55)= vlLightLock
	Lampz.MassAssign(55)= blLightLock
	Lampz.Callback(55) = "DisableLighting plLightLock, 300,"
	
	Lampz.MassAssign(56)= Light021
	Lampz.MassAssign(56)= vLight021
	Lampz.MassAssign(56)= bLight021
	Lampz.Callback(56) = "DisableLighting pLight021, 300,"
	
	Lampz.MassAssign(57)= lLoopRight
	Lampz.MassAssign(57)= vlLoopRight
	Lampz.MassAssign(57)= blLoopRight
	Lampz.Callback(57) = "DisableLighting plLoopRight, 380,"
	
	Lampz.MassAssign(58)= lBattle
	Lampz.MassAssign(58)= vlBattle
	Lampz.MassAssign(58)= blBattle
	Lampz.Callback(58) = "DisableLighting plBattle, 300,"
	
	Lampz.MassAssign(59)= lRampCenter
	Lampz.MassAssign(59)= vlRampCenter
	Lampz.MassAssign(59)= blRampCenter
	Lampz.Callback(59) = "DisableLighting plRampCenter, 380,"
	
	Lampz.MassAssign(60)= lLock
	Lampz.MassAssign(60)= vlLock
	Lampz.MassAssign(60)= blLock
	Lampz.Callback(60) = "DisableLighting plLock, 380,"
	
	Lampz.MassAssign(61)= lJackpot
	Lampz.MassAssign(61)= vlJackpot
	Lampz.MassAssign(61)= blJackpot
	Lampz.Callback(61) = "DisableLighting plJackpot, 300,"
	
	Lampz.MassAssign(62)= lUnderworld
	Lampz.MassAssign(62)= vlUnderworld
	Lampz.MassAssign(62)= blUnderworld
	Lampz.Callback(62) = "DisableLighting plUnderworld, 300,"
	
	Lampz.MassAssign(63)= Light040
	Lampz.MassAssign(63)= vLight040
	Lampz.MassAssign(63)= bLight040
	Lampz.Callback(63) = "DisableLighting pLight040, 300,"
	
	Lampz.MassAssign(64)= Light041
	Lampz.MassAssign(64)= vLight041
	Lampz.MassAssign(64)= bLight041
	Lampz.Callback(64) = "DisableLighting pLight041, 300,"
	
	Lampz.MassAssign(65)= Light042
	Lampz.MassAssign(65)= vLight042
	Lampz.MassAssign(65)= bLight042
	Lampz.Callback(65) = "DisableLighting pLight042, 300,"
	
	Lampz.MassAssign(66)= Light043
	Lampz.MassAssign(66)= vLight043
	Lampz.MassAssign(66)= bLight043
	Lampz.Callback(66) = "DisableLighting pLight043, 300,"
	
	Lampz.MassAssign(67)= Light020
	Lampz.MassAssign(67)= vLight020
	Lampz.MassAssign(67)= bLight020
	Lampz.Callback(67) = "DisableLighting pLight020, 300,"

	Lampz.MassAssign(68)= lTargetX1
	Lampz.MassAssign(68)= vlTargetX1
	Lampz.MassAssign(68)= blTargetX1
	Lampz.Callback(68) = "DisableLighting plTargetX1, 300,"

	Lampz.MassAssign(69)= lTargetX2
	Lampz.MassAssign(69)= vlTargetX2
	Lampz.MassAssign(69)= blTargetX2
	Lampz.Callback(69) = "DisableLighting plTargetX2, 300,"

	Lampz.MassAssign(70)= lTargetX3
	Lampz.MassAssign(70)= vlTargetX3
	Lampz.MassAssign(70)= blTargetX3
	Lampz.Callback(70) = "DisableLighting plTargetX3, 300,"
	
	Lampz.MassAssign(71)= lTargetX4
	Lampz.MassAssign(71)= vlTargetX4
	Lampz.MassAssign(71)= blTargetX4
	Lampz.Callback(71) = "DisableLighting plTargetX4, 300,"

	'Solonoid Controlller Flasher 173 in manual.
'	Lampz.MassAssign(72)= LBumper1
'	Lampz.MassAssign(72)= vLBumper1
'	Lampz.Callback(72) = "DisableLighting f173bulb, 300,"

	'Lamp 37 in manual
	Lampz.MassAssign(73)= LBumper2
	Lampz.MassAssign(73)= vLBumper2
	Lampz.Callback(73) = "DisableLighting l37bulb, 300,"

	'Lamp 38 in manual
	Lampz.MassAssign(74)= LBumper3
	Lampz.MassAssign(74)= vLBumper3
	Lampz.Callback(74) = "DisableLighting l38bulb, 300,"

    Lampz.MassAssign(75)= l83
    Lampz.Callback(75) = "FadeMaterialColoredBulb pFBase83, MaterialRedArray, "
    Lampz.Callback(75) = "FadeMaterialColoredFiliment pFiliment83, MaterialRedArray, "

    Lampz.MassAssign(76)= l84
    Lampz.Callback(76) = "FadeMaterialColoredBulb pFBase84, MaterialYellowArray, "
    Lampz.Callback(76) = "FadeMaterialColoredFiliment pFiliment84, MaterialYellowArray, "

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


Function LampNameToIdx(Name)	' TBD Use a Dictionary 
	LampNameToIdx = -1
	Select Case Name
		Case "LightShootAgain": LampNameToIdx=1
		Case "lModeIcarus": LampNameToIdx=2
		Case "lModeHallowed": LampNameToIdx=3
		Case "lModeRime": LampNameToIdx=4
		Case "lModeFear": LampNameToIdx=5
		Case "lModeAces": LampNameToIdx=6
		Case "lMode2M2M": LampNameToIdx=7
		Case "Light004": LampNameToIdx=8
		Case "Light003": LampNameToIdx=9
		Case "Light001": LampNameToIdx=10
		Case "Light022": LampNameToIdx=11
		Case "Light002": LampNameToIdx=12
		Case "lPowerPops": LampNameToIdx=13
		Case "lPowerTargets": LampNameToIdx=14
		Case "lPowerSpinner": LampNameToIdx=15
		Case "lPowerOrbits": LampNameToIdx=16
		Case "lPowerRamps": LampNameToIdx=17
		Case "Light014": LampNameToIdx=18
		Case "Light013": LampNameToIdx=19
		Case "Light012": LampNameToIdx=20
		Case "Light007": LampNameToIdx=21
		Case "Light006": LampNameToIdx=22
		Case "Light072": LampNameToIdx=23
		Case "Light073": LampNameToIdx=24
		Case "lNOTB": LampNameToIdx=25
		Case "lCardMummy": LampNameToIdx=26
		Case "lCardCyborg": LampNameToIdx=27
		Case "lCardTrooper": LampNameToIdx=28
		Case "lCard2M2M": LampNameToIdx=29
		Case "Light005": LampNameToIdx=30
		Case "lPowerSpinnerArrow1": LampNameToIdx=31
		Case "lSpinnerLeft": LampNameToIdx=32
		Case "lOrbArrow": LampNameToIdx=33
		Case "Light045": LampNameToIdx=34
		Case "ORBLight": LampNameToIdx=35
		Case "lRampArrow": LampNameToIdx=36
		Case "lRampRight": LampNameToIdx=37
		Case "lOrbitArrow2": LampNameToIdx=38
		Case "lOrbitRight": LampNameToIdx=39
		Case "lExtraBall": LampNameToIdx=40
		Case "Light048": LampNameToIdx=41
		Case "lRampLeft": LampNameToIdx=42
		Case "lLoopLeft": LampNameToIdx=43
		Case "lPowerSpinnerArrow2": LampNameToIdx=44
		Case "lOrbitArrow": LampNameToIdx=45
		Case "lOrbitLeft": LampNameToIdx=46
		Case "lCaptiveBall": LampNameToIdx=47
		Case "lMummyM": LampNameToIdx=48
		Case "lMummyU": LampNameToIdx=49
		Case "lMummyM2": LampNameToIdx=50
		Case "lMummyM3": LampNameToIdx=51
		Case "lMummyY": LampNameToIdx=52
		Case "lBonusX": LampNameToIdx=53
		Case "lLightOrb": LampNameToIdx=54
		Case "lLightLock": LampNameToIdx=55
		Case "Light021": LampNameToIdx=56
		Case "lLoopRight": LampNameToIdx=57
		Case "lBattle": LampNameToIdx=58
		Case "lRampCenter": LampNameToIdx=59
		Case "lLock": LampNameToIdx=60
		Case "lJackpot": LampNameToIdx=61
		Case "lUnderworld": LampNameToIdx=62
		Case "Light040": LampNameToIdx=63
		Case "Light041": LampNameToIdx=64
		Case "Light042": LampNameToIdx=65
		Case "Light043": LampNameToIdx=66
		Case "Light020": LampNameToIdx=67
		Case "lTargetX1": LampNameToIdx=68
		Case "lTargetX2": LampNameToIdx=69
		Case "lTargetX3": LampNameToIdx=70
		Case "lTargetX4": LampNameToIdx=71
'		Case "xxxxxxxxx": LampNameToIdx=72
		Case "LBumper2": LampNameToIdx=73
		Case "LBumper3": LampNameToIdx=74
		Case "l83": LampNameToIdx=75
		Case "l84": LampNameToIdx=76
	End Select 
End Function 



'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.14 - apophis - added IsLight property to the class
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public IsLight(140)					'apophis
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
			IsLight(x) = False   		'apophis
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''WriteToLog "     ", Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
				if typename(aInput) = "Light" then IsLight(aIdx) = True   'apophis - If first object in array is a light, this will be set true
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Function GetLampName(aIdx)							' Daphishbowl added
		if typename(Obj(aIdx)) = "Light" then 
			GetLampName=Obj(aIdx).name
		else 
			GetLampName=""
		End if 
	End Function

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''WriteToLog "     ", debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************




'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and   
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target. 
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield 
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded 
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' NOTE: MAKE SURE YOU ADD "DoDTAnim" to your game timer with the RollingUpdate function


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT1, DT2, DT3

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

Set DT1 = (new DropTarget)(Target004, Target004a, Target004p, 1, 0, false)
Set DT2 = (new DropTarget)(Target005, Target005a, Target005p, 2, 0, false)
Set DT3 = (new DropTarget)(Target006, Target006a, Target006p, 3, 0, false)


Dim DTArray
DTArray = Array(DT1, DT2, DT3)
Dim DTArray_IsDropped(3):DTArray_IsDropped(0)=0:DTArray_IsDropped(1)=0:DTArray_IsDropped(2)=0


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "fx_droptarget" 'Drop Target Drop sound
Const DTResetSound = "fx_resetdrop" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance (Default = 0.2)


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Function DTIsDropped(Switch)
	Dim i
	i = DTArrayID(switch)
	DTIsDropped = DTArray_IsDropped(i)
'WriteToLog "     ", "DTArray(i).prim.transz:" & DTArray(i).prim.transz & " " & DTArray(i).animate
'	if DTArray(i).prim.transz = -DTDropUnits or DTArray(i).animate>=1 then 	
'		DTIsDropped=True
'	else 
'		DTIsDropped=False 
'	End if 

End Function

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	DTArray_IsDropped(i)=1

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
'WriteToLog "     ", "DTHit:(" & i & ") " & DTArray(i).animate
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i

'WriteToLog "     ", "RAISE"
	i = DTArrayID(switch)
	DTArray_IsDropped(i)=0

	DTArray(i).animate = -1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	DTArray_IsDropped(i)=1

	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3

	' Adjust the following to kill momentum of ball
	aBall.velx=aBall.velx*0.7
	aBall.vely=aBall.vely*0.7
End Sub


'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		DTCheckBrick = 4
	Else 
		DTCheckBrick = 0
	End If
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	rangle = prim.rotz * PI / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
	If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
	elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim BOT, b
			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
					BOT(b).velz = 10 ' Default=20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1

	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


' Used for drop targets
'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'******************************************************
'****  END DROP TARGETS
'******************************************************



'**********************************************************
'     JP's Flasher Fading for VPX original tables
'       (Based on Pacdude's Fading Light System)
'
'		Using: SetFlash to flash flashers 
'
'**********************************************************

Dim FadingState(100), FlashLevel(100)
InitLamps() ' turn off the lights and flashers and reset them to the default parameters
Sub LampTimer_Timer()
'    Flash 1, flasher001

'    Lampm 2, LFlasher002
'    Flashm 2, flasher002a
'    Flash 2, flasher002

'    Flash 3, flasher003

    Flash 4, flasher004

    Lampm 5, LFlasher005
    Flash 5, flasher005

    Flash 6, flasher006

    Lampm 7, LFlasher006
    Lampm 7, LFlasher007
    Flashm 7, flasher007
    Flash 7, flasher008

'    Lampm 9, LFlasher009
'    Flash 9, flasher009

'    Lampm 10, LFlasher010
'    Flash 10, flasher010

    Lamp 11, ORBLight

'    Flashm 12, flasher012a
'    Flashm 12, flasher012b
'    Flash 12, flasher012

    Flash 13, flasher013
    Flash 14, flasher014
'	Flash 15, flasher015
    Flash 16, FlasherFace
End Sub
Sub InitLamps()
    Dim x
    LampTimer.Interval = 40 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 100
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetFlash(MyLamp, nr, TotalPeriod, BlinkPeriod) 'similar to FlashForms, works on all kind of objects with the fading light systems
    Dim steps
'WriteToLog "     ", "SetFlash:" & MyLamp.Name & " " & MyLamp.UserValue
    ' Store all blink information
    steps = (TotalPeriod / BlinkPeriod + .5) \ 2 'Number of ON/OFF steps to perform
    steps = steps + nr / 100                     'steps + fading lights number
    MyLamp.UserValue = steps                     'Store # of blinks & Lamp number

    ' Start the flasher timer
    MyLamp.TimerInterval = BlinkPeriod
    MyLamp.TimerEnabled = 0
    MyLamp.TimerEnabled = 1
'WriteToLog "     ", "Sub " & MyLamp.Name & "_Timer:" & "SetLamp (me.UserValue - INT(me.UserValue))*100, me.UserValue MOD 2:me.UserValue= me.UserValue -1:If me.UserValue < 0 then Me.TimerEnabled=0:End If:End Sub"

    ExecuteGlobal "Sub " & MyLamp.Name & "_Timer:" & "SetLamp 100*(" & MyLamp.Name & ".UserValue - INT(" & MyLamp.Name & ".UserValue)), " & MyLamp.Name & ".UserValue MOD 2:" & MyLamp.Name & ".UserValue= " & MyLamp.Name & ".UserValue -1:If " & MyLamp.Name & ".UserValue < 0 then " & MyLamp.Name & ".TimerEnabled=0:End If:End Sub"
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' standard VPX objects

' Lights: used for VPX standard lights,
' the fading is handled by VPX itself,
' they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub

' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)
'
'' Reels
'
'Sub Reel(nr, object)
'    Select Case FadingState(nr)
'        Case 4:object.SetValue 1:FadingState(nr) = 0
'        Case 3:object.SetValue 2:FadingState(nr) = 2
'        Case 2:object.SetValue 3:FadingState(nr) = 1
'        Case 1:object.SetValue 0:FadingState(nr) = 0
'    End Select
'End Sub
'
'Sub Reelm(nr, object)
'    Select Case FadingState(nr)
'        Case 4:object.SetValue 1
'        Case 3:object.SetValue 2
'        Case 2:object.SetValue 3
'        Case 1:object.SetValue 0
'    End Select
'End Sub
'
''Texts
'
'Sub Text(nr, object, message)
'    Select Case FadingState(nr)
'        Case 4:object.Text = message:FadingState(nr) = 0
'        Case 3:object.Text = "":FadingState(nr) = 0
'    End Select
'End Sub
'
'Sub Textm(nr, object, message)
'    Select Case FadingState(nr)
'        Case 4:object.Text = message
'        Case 3:object.Text = ""
'    End Select
'End Sub
'
''Walls and mostly Primitives used as 4 step fading lights
''a,b,c,d are the images used from on to off
'
'Sub FadeObj(nr, object, a, b, c, d)
'    Select Case FadingState(nr)
'        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
'        Case 3:object.image = b:FadingState(nr) = 2
'        Case 2:object.image = c:FadingState(nr) = 1
'        Case 1:object.image = d:FadingState(nr) = 0
'    End Select
'End Sub
'
'Sub FadeObjm(nr, object, a, b, c, d)
'    Select Case FadingState(nr)
'        Case 4:object.image = a
'        Case 3:object.image = b
'        Case 2:object.image = c
'        Case 1:object.image = d
'    End Select
'End Sub
'
'Sub NFadeObj(nr, object, a, b)
'    Select Case FadingState(nr)
'        Case 4:object.image = a:FadingState(nr) = 0 'off
'        Case 3:object.image = b:FadingState(nr) = 0 'on
'    End Select
'End Sub
'
'Sub NFadeObjm(nr, object, a, b)
'    Select Case FadingState(nr)
'        Case 4:object.image = a
'        Case 3:object.image = b
'    End Select
'End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
' the flashers do not work if are using the fading lights system, use SetFlash instead
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version
    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub


Sub SetSlowPulse(Light)
	dim x:
	x=LampNameToIdx(Light.Name)
	Lampz.FadeSpeedUp(x) = 0.1
	Lampz.FadeSpeedDown(x) = 0.1 
'	for x = 0 to 140
'		if Lampz.GetLampName(x)=light.name then 
'			Lampz.FadeSpeedUp(x) = 0.1
'			Lampz.FadeSpeedDown(x) = 0.1 
'		End if 
'	Next

	Light.blinkInterval=900
	Light.fadespeedUp=0.1
	Light.fadespeedDown=0.1
'	Light.falloff=40
End Sub

Sub SetDefPulse(Light)
	x=LampNameToIdx(Light.Name)
	Lampz.FadeSpeedUp(x) = 1/2
	Lampz.FadeSpeedDown(x) = 1/10 
'	for x = 0 to 140
'		if Lampz.GetLampName(x)=light.name then 
'			Lampz.FadeSpeedUp(x) = 1/2
'			Lampz.FadeSpeedDown(x) = 1/10 
'		End if 
'	Next

	Light.blinkInterval=400 '125
	Light.fadespeedUp=40
	Light.fadespeedDown=40
'	Light.falloff=40
End Sub

Sub SetFastPulse(Light)
	x=LampNameToIdx(Light.Name)
	Lampz.FadeSpeedUp(x) = 1/2
	Lampz.FadeSpeedDown(x) = 1/10 
'	for x = 0 to 140
'		if Lampz.GetLampName(x)=light.name then 
'			Lampz.FadeSpeedUp(x) = 1/2
'			Lampz.FadeSpeedDown(x) = 1/10 
'		End if 
'	Next

	Light.blinkInterval=125
	Light.fadespeedUp=40
	Light.fadespeedDown=40
End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************

Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, noColor, almond, cyan, teal

almond = 13
teal = 12
cyan = 11
red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1
noColor = -1 

Sub SetLightColor(n, col, stat)		' States: 0=Off, 1=On, 2=Flash, 3=Flash On, 4=Flash Off
	SetLightColorTimed n, col, stat, 500
End Sub 


'updated by apophis to work with Lampz and 3D insert primitives
Sub SetLightColorTimed(n, col, stat, msec) ' n = light, col = color, ' States: 0=Off, 1=On, 2=Flash, 3=Flash On, 4=Flash Off
	if TypeName(n)="LightDummy" then exit sub 	' Fake Light, cant set the color 
WriteToLog "     ", "SetLightColorTimed:" & n.name & " " & col
	dim vL, pL
	Set vL = eval("v" & n.name)
	Set pL = eval("p" & n.name)

	'UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity, OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive, float elasticity, float elasticityFalloff, float friction, float scatterAngle)

    Select Case col
        Case 0
            vL.color = RGB(18, 0, 0)
            vL.colorfull = RGB(255, 0, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(255, 0, 0),0,0,False,True,0,0,0,0
        Case red
            vL.color = RGB(32, 0, 0)
            vL.colorfull = RGB(224, 0, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(224, 0, 0),0,0,False,True,0,0,0,0
        Case orange
            vL.color = RGB(18, 3, 0)
            vL.colorfull = RGB(255, 64, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(255, 64, 0),0,0,False,True,0,0,0,0
        Case amber
            vL.color = RGB(193, 49, 0)
            vL.colorfull = RGB(255, 153, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(255, 153, 0),0,0,False,True,0,0,0,0
        Case yellow
            vL.color = RGB(18, 18, 0)
            vL.colorfull = RGB(255, 255, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(255, 255, 0),0,0,False,True,0,0,0,0
        Case darkgreen
            vL.color = RGB(0, 8, 0)
            vL.colorfull = RGB(0, 64, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 64, 0),0,0,False,True,0,0,0,0
        Case green
            vL.color = RGB(0, 16, 0)
            vL.colorfull = RGB(0, 128, 0)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 128, 0),0,0,False,True,0,0,0,0
        Case cyan
            vL.color = RGB(0, 18, 18)
            vL.colorfull = RGB(0, 255, 255)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 255, 255),0,0,False,True,0,0,0,0
        Case teal
            vL.color = RGB(0, 18, 18)
            vL.colorfull = RGB(0, 124, 128)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 124, 128),0,0,False,True,0,0,0,0
        Case blue
            vL.color = RGB(0, 0, 128)
            vL.colorfull = RGB(0, 128, 255)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 128, 255),0,0,False,True,0,0,0,0
        Case darkblue
            vL.color = RGB(0, 4, 18)
            vL.colorfull = RGB(0, 64, 64)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(0, 64, 64),0,0,False,True,0,0,0,0
        Case purple
            vL.color = RGB(64, 0, 96)
            vL.colorfull = RGB(128, 0, 192)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(128, 0, 192),0,0,False,True,0,0,0,0
        Case almond
            vL.color = RGB(193, 91, 0)
            vL.colorfull = RGB(239, 222, 205)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(239, 222, 205),0,0,False,True,0,0,0,0
        Case white
            vL.color = RGB(193, 91, 0)
            vL.colorfull = RGB(255, 252, 224)
			UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(255, 252, 224),0,0,False,True,0,0,0,0
    End Select
    If stat <> -1 Then
        n.State = 0
		if stat=3 then  	' Flash then go to On
			SetFastPulse n
			n.State = 2
			vpmTimer.addTimer msec, n.name & ".state =1:SetDefPulse " & n.name & " '"
		elseif stat=4 then  	' Flash then go to Off
			SetFastPulse n
			n.State = 2
			vpmTimer.addTimer msec, n.name & ".state =0:SetDefPulse " & n.name & " '"
		Else 
			n.State = stat
		End if 
    End If
End Sub

Sub SetLightColorRGB(n, cRed, cGreen, cBlue, fRed, fGreen, fBlue) ' n = light, col = color, ' States: 0=Off, 1=On, 2=Flash, 3=Flash On, 4=Flash Off
	dim vL, pL
	Execute "set vL=v" & n.name
	Execute "set pL=p" & n.name

	vL.color = RGB(cRed, cGreen, cBlue)
	vL.colorfull = RGB(fRed, fGreen, fBlue)
	UpdateMaterial pL.material,0,0,0,0,0,0,1,RGB(fRed, fGreen, fBlue),0,0,False,True,0,0,0,0
End Sub 


Sub SetGILightColor(n, col, stat)		' States: 0=Off, 1=On, 2=Flash, 3=Flash On, 4=Flash Off
	SetGILightColorTimed n, col, stat, 500
End Sub 


Sub SetGILightColorTimed(n, col, stat, msec) ' n = light, col = color, ' States: 0=Off, 1=On, 2=Flash, 3=Flash On, 4=Flash Off
'WriteToLog "     ", n.name & " " & col

    Select Case col
        Case 0
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case red
            n.color = RGB(32, 0, 0)
            n.colorfull = RGB(224, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 4, 18)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
    End Select
    If stat <> -1 Then
        n.State = 0
		if stat=3 then  	' Flash then go to On
			SetFastPulse n
			n.State = 2
			vpmTimer.addTimer msec, n.name & ".state =1:SetDefPulse " & n.name & " '"
		elseif stat=4 then  	' Flash then go to Off
			SetFastPulse n
			n.State = 2
			vpmTimer.addTimer msec, n.name & ".state =0:SetDefPulse " & n.name & " '"
		Else 
			n.State = stat
		End if 
    End If
End Sub


'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights,RainbowMode:RainbowMode=-1
Dim myRainbowCollection
Dim MyRainboxIdx 

'StartRainbowMode kModeTrooper, kLightRampCenter
Sub StartRainbowMode(mode, lIndex) 'uses a collection as parameter
	RainbowMode=mode 
	
	set myRainbowCollection=new cvpmDictionary
	myRainbowCollection.Add LightMap(lIndex), 0
	SSetLightColor mode, lIndex, white, 1
	MyRainboxIdx=lIndex

	RainbowTimer.interval=10
	StartRainbow2 myRainbowCollection, 50
End Sub 

Sub StartRainbow(n) 'uses a collection as parameter
	RainbowTimer.interval=100
	StartRainbow2 n, 5
End Sub 

Sub StartRainbow2(n, speed) 'uses a collection as parameter
    set RainbowLights = n
    RGBStep = 0
	RGBFactor = speed
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
	if RainbowTimer.Enabled = False then exit sub 		' Nothing to do 
    RainbowTimer.Enabled = 0
	if RainbowMode=-1 then 
		For each x in RainbowLights
			SetLightColor x, white, 0
		Next
	else 
		SSetLightColor RainbowMode, MyRainboxIdx, white, 0
	End if 
	RainbowMode=-1 
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen> 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed <0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue> 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen <0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed> 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue <0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
	if RainbowMode=-1 then 
		For each obj in RainbowLights
			obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
			obj.colorfull = RGB(rRed, rGreen, rBlue)
		Next
	else 
		For each obj in RainbowLights.keys
			SetLightColorRGB obj, rRed \ 10, rGreen \ 10, rBlue \ 10, rRed, rGreen, rBlue
		Next
	End if 
End Sub


Sub StartAttractMode(bReset)
WriteToLog "     ", "StartAttractMode"
	TurnOffDofUndercab
	DOF 200, DOFon 
	pClearEverything
	GiOff
    StartLightSeq
    DMDFlush
    'ShowTableInfo
    StartRainbow aLightArrows
	AttractTimer.UserValue = 0
    AttractTimer.Enabled = 1
	pAttractStart(bReset)
End Sub

Sub AttractTimer_Timer
    Dim tmp
	AttractTimer.Enabled=0		' Reset in case they pressed a key 
	AttractTimer.Enabled=1
	AttractTimer.UserValue = 0

    tmp = INT(RND * 15)
    Select Case tmp
        Case 0:PlaySoundVol "attract_ahoy", VolDef
        Case 1:PlaySoundVol "attract_battlefield", VolDef
        Case 2:PlaySoundVol "attract_beast", VolDef
        Case 3:PlaySoundVol "attract_come", VolDef
        Case 4:PlaySoundVol "attract_glass", VolDef
		Case 5:PlaySoundVol "attract_hangman", VolDef
		Case 6:PlaySoundVol "attract_hurry", VolDef
		Case 7:PlaySoundVol "attract_letsgo", VolDef
		Case 8:PlaySoundVol "attract_scared", VolDef
		Case 9:PlaySoundVol "attract_soul", VolDef
		Case 10:PlaySoundVol "attract_up", VolDef
		Case 11:PlaySoundVol "attract_waiting!", VolDef
		Case 12:PlaySoundVol "attract_waiting", VolDef
		Case 13:PlaySoundVol "attract_welcome", VolDef
		Case 14:PlaySoundVol "attract_wretch", VolDef
    End Select
End Sub


Sub RandomVOSoundEndOfBall  ' Sidelane Drain
	if(bGameInPLay = True)AND(Tilted = False) Then
		If (bBallSaverActive = False) and (BallsOnPlayfield-BallsInRealLock-1 = 0) Then
			PlaySoundVol "vo_drain" & INT(RND * 8)+1, VolDef
		End if  
	End if 
End Sub

Sub RandomVOSoundEndOfBall2	' Center Drain 
	If LastSwitchHit<>"Trigger001" and LastSwitchHit<>"Trigger004" then 
		Select Case INT(RND * 16)
			Case 0:PlaySoundVol "vo_dobetterthanthat", VolDef
			Case 1:PlaySoundVol "vo_hahaha", VolDef
			Case 2, 12:PlaySoundVol "vo_hahaha2", VolDef
			Case 3, 13:PlaySoundVol "vo_hahaha3", VolDef
			Case 4, 14:PlaySoundVol "vo_hohoho", VolDef
			Case 5, 15:PlaySoundVol "vo_hohoho2", VolDef
			Case 6:PlaySoundVol "vo_keepitup", VolDef
			Case 7:PlaySoundVol "vo_firsttime", VolDef
			Case 8:PlaySoundVol "vo_lookatthatscore", VolDef
			Case 9:PlaySoundVol "vo_excellentshot", VolDef
			Case 10:PlaySoundVol "vo_goodshot", VolDef
			Case 11:PlaySoundVol "vo_flippers", VolDef
			case 12:PlaySoundVol "vo_notagoodplayer", VolDef
		End Select
	End if 
End Sub

Sub StopAttractMode
	TurnOffDofUndercab	
	DOF 201, DOFon 'Regular play Gold Lighting for undercab
    GiOn
    DMDScoreNow
    LightSeqAttract.StopPlay
	ManualAttract_Stop
    StopRainbow
    AttractTimer.Enabled = 0
	pDMDStartGame
End Sub


Sub ManualAttract_Start()
	SetLightColor lCaptiveBall, red, 0
	tmrLightSeqAttract_four=0
	tmrLightSeqAttract_six=0
	tmrLightSeqAttract_Loop=0
	tmrLightSeqAttract_Orbit1=0
	tmrLightSeqAttract.Interval = 100
	tmrLightSeqAttract.Enabled = True 
End Sub 

dim tmrLightSeqAttract_six
dim tmrLightSeqAttract_four
dim tmrLightSeqAttract_Loop
dim tmrLightSeqAttract_Orbit1
Sub tmrLightSeqAttract_Timer()

	' 3 lights
	Select case tmrLightSeqAttract_Orbit1
		case 0:lPowerSpinnerArrow2.state=1:lOrbitArrow.state=0:lOrbitLeft.state=0
				lBonusX.state=1:lLightOrb.state=0:lLightLock.state=0
		case 1:lPowerSpinnerArrow2.state=1:lOrbitArrow.state=1:lOrbitLeft.state=0
				lBonusX.state=1:lLightOrb.state=1:lLightLock.state=0
		case 2:lPowerSpinnerArrow2.state=0:lOrbitArrow.state=1:lOrbitLeft.state=0
				lBonusX.state=0:lLightOrb.state=1:lLightLock.state=0
		case 3:lPowerSpinnerArrow2.state=0:lOrbitArrow.state=1:lOrbitLeft.state=1
				lBonusX.state=0:lLightOrb.state=1:lLightLock.state=1
		case 4:lPowerSpinnerArrow2.state=0:lOrbitArrow.state=0:lOrbitLeft.state=1
				lBonusX.state=0:lLightOrb.state=0:lLightLock.state=1
		case 5:lPowerSpinnerArrow2.state=1:lOrbitArrow.state=0:lOrbitLeft.state=1
				lBonusX.state=1:lLightOrb.state=0:lLightLock.state=1
	End Select 
	tmrLightSeqAttract_Orbit1=tmrLightSeqAttract_Orbit1+1
	tmrLightSeqAttract_Orbit1=tmrLightSeqAttract_Orbit1 mod 5

	' 4 lights
	Select case tmrLightSeqAttract_four
		case 0:lCardMummy.state=1:lCardCyborg.state=0:lCardTrooper.state=0:lCard2M2M.state=0
		case 1:lCardMummy.state=1:lCardCyborg.state=1:lCardTrooper.state=0:lCard2M2M.state=0
		case 2:lCardMummy.state=0:lCardCyborg.state=1:lCardTrooper.state=0:lCard2M2M.state=0
		case 3:lCardMummy.state=0:lCardCyborg.state=1:lCardTrooper.state=1:lCard2M2M.state=0
		case 4:lCardMummy.state=0:lCardCyborg.state=0:lCardTrooper.state=1:lCard2M2M.state=0
		case 5:lCardMummy.state=0:lCardCyborg.state=0:lCardTrooper.state=1:lCard2M2M.state=1
		case 6:lCardMummy.state=0:lCardCyborg.state=0:lCardTrooper.state=0:lCard2M2M.state=1
		case 7:lCardMummy.state=1:lCardCyborg.state=0:lCardTrooper.state=0:lCard2M2M.state=1
	End Select 
	tmrLightSeqAttract_four=tmrLightSeqAttract_four+1
	tmrLightSeqAttract_four=tmrLightSeqAttract_four mod 7


	' 5 lights 
	Select case tmrLightSeqAttract_Loop 
		case 0:Light040.state=1:Light041.state=0:Light042.state=0:Light043.state=0:Light020.state=0
			   Light022.state=1:lLoopLeft.state=0
		case 1:Light040.state=1:Light041.state=1:Light042.state=0:Light043.state=0:Light020.state=0
			   Light022.state=1:lLoopLeft.state=1
		case 2:Light040.state=0:Light041.state=1:Light042.state=0:Light043.state=0:Light020.state=0
			   Light022.state=0:lLoopLeft.state=1
		case 3:Light040.state=0:Light041.state=1:Light042.state=1:Light043.state=0:Light020.state=0
			   Light022.state=0:lLoopLeft.state=0
		case 4:Light040.state=0:Light041.state=0:Light042.state=1:Light043.state=0:Light020.state=0
		case 5:Light040.state=0:Light041.state=0:Light042.state=1:Light043.state=1:Light020.state=0
		case 6:Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=1:Light020.state=0
		case 7:Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=1:Light020.state=1
		case 8:Light040.state=1:Light041.state=0:Light042.state=0:Light043.state=0:Light020.state=1
	End Select 
	tmrLightSeqAttract_Loop=tmrLightSeqAttract_Loop+1
	tmrLightSeqAttract_Loop=tmrLightSeqAttract_Loop mod 8



	' 6 lights 
	Select case tmrLightSeqAttract_six 
		case 0:lMummyY.state=1:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=1:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=1:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0

		case 1:lMummyY.state=1:lMummyM3.state=1:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=1:lPowerTargets.state=1:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=1:lModeHallowed.state=1:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0

		case 2:lMummyY.state=0:lMummyM3.state=1:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=1:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=1:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0

		case 3:lMummyY.state=0:lMummyM3.state=1:lMummyM2.state=1:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=1:lPowerSpinner.state=1:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=1:lModeRime.state=1:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0

		case 4:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=1:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=1:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=1:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0

		case 5:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=1:lMummyU.state=1:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=1:lPowerOrbits.state=1:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=1:lModeFear.state=1:lModeAces.state=0:lMode2M2M.state=0

		case 6:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=1:lMummyM.state=0:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=1:lPowerRamps.state=0:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=1:lModeAces.state=0:lMode2M2M.state=0

		case 7:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=1:lMummyM.state=1:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=1:lPowerRamps.state=1:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=1:lModeAces.state=1:lMode2M2M.state=0

		case 8:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=1:lCaptiveBall.state=0
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=1:Light014.state=0
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=1:lMode2M2M.state=0

		case 9:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=1:lCaptiveBall.state=1
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=1:Light014.state=1
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=1:lMode2M2M.state=1

		case 10:lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=1
			   lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=1
			   lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=1

		case 11:lMummyY.state=1:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=1
			   lPowerPops.state=1:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=1
			   lModeIcarus.state=1:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=1
	End Select 
	tmrLightSeqAttract_six=tmrLightSeqAttract_six+1
	tmrLightSeqAttract_six=tmrLightSeqAttract_six mod 11

End Sub 


Sub ManualAttract_Stop()
	tmrLightSeqAttract.Enabled = False  
	Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=0:Light020.state=0
	Light022.state=0:lLoopLeft.state=0

	lCardMummy.state=0:lCardCyborg.state=0:lCardTrooper.state=0:lCard2M2M.state=0

	lPowerSpinnerArrow2.state=0:lOrbitArrow.state=0:lOrbitLeft.state=0
	lBonusX.state=0:lLightOrb.state=0:lLightLock.state=0

	lMummyY.state=0:lMummyM3.state=0:lMummyM2.state=0:lMummyU.state=0:lMummyM.state=0:lCaptiveBall.state=0
	lPowerPops.state=0:lPowerTargets.state=0:lPowerSpinner.state=0:lPowerOrbits.state=0:lPowerRamps.state=0:Light014.state=0
	lModeIcarus.state=0:lModeHallowed.state=0:lModeRime.state=0:lModeFear.state=0:lModeAces.state=0:lMode2M2M.state=0
End Sub 

Sub StartLightSeq()

	ManualAttract_Start

    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'Sub LightSeqSkillshot_PlayDone()
'    LightSeqSkiEDDIE_targetllshot.Play SeqAllOff
'End Sub


' **********************************************************************
' ***********  Daphishbowl Mode Light - Supports stacking
' **********************************************************************

Dim lPharaohTarget:set lPharaohTarget = new LightDummy:lPharaohTarget.name="lPharaohTarget"
Dim lSpinnerRight:set lSpinnerRight = new LightDummy:lSpinnerRight.name="lSpinnerRight"
Dim lSpinnerStop:set lSpinnerStop = new LightDummy:lSpinnerStop.name="lSpinnerStop"
Dim lSpinnerStopL:set lSpinnerStopL = new LightDummy:lSpinnerStopL.name="lSpinnerStopL"
Dim lSwitch:set lSwitch = new LightDummy:lSwitch.name="lSwitch"
Dim lBumper:set lBumper = new LightDummy:lBumper.name="lBumper"
Dim lLightOrbitLeft2:set lLightOrbitLeft2 = new LightDummy:lLightOrbitLeft2.name="lLightOrbitLeft2"
Dim lLightOrbitRight2:set lLightOrbitRight2 = new LightDummy:lLightOrbitRight2.name="lLightOrbitRight2"
Dim lLightUnderworld:set lLightUnderworld = new LightDummy:lLightUnderworld.name="lLightUnderworld"
Dim lLightLoopLeft2:set lLightLoopLeft2 = new LightDummy:lLightLoopLeft2.name="lLightLoopLeft2"
Dim lLightLoopRight2:set lLightLoopRight2 = new LightDummy:lLightLoopRight2.name="lLightLoopRight2"
Dim lLightSlingLeft:set lLightSlingLeft = new LightDummy:lLightSlingLeft.name="lLightSlingLeft"
Dim lLightSlingRight:set lLightSlingRight = new LightDummy:lLightSlingRight.name="lLightSlingRight"

Class LightDummy
	private lname
	private lstate
	private lcolor
	private lcolorfull
	Public Property Get name()
		name = lname
	End Property
	Public Property Let name(value)
		lname = value
	End Property
	Public Property Get state()
		state = lstate
	End Property
	Public Property Let state(value)
		lstate = value
	End Property
	Public Property Get color()
		color = lcolor
	End Property
	Public Property Let color(value)
		color = value
	End Property	
	Public Property Get colorfull()
		colorfull = lcolorfull
	End Property
	Public Property Let colorfull(value)
		colorfull = value
	End Property
End Class 


' Define Variable for all the lights that are tied to modes
Const kLightRampLeft	= 0
Const kLightRampRight	= 1
Const kLightRampCenter	= 2
const kLightLoopLeft	= 3				' Partial Loop 
const kLightLoopRight	= 4				' Partial Loop
const kLightOrbitLeft	= 5
const kLightOrbitRight	= 6
const kLightSpinnerLeft = 7
const kLightSpinnerRight= 8
const kLightSpinnerStop = 9
const kLightSpinnerStopL= 10
const kLightPharaohTarget=11
const kLightCaptiveBall =12
const kLightLock		=13
const kLightOrb			=14
const kLightBonusX		=15
const kLightTargetX1	=16
const kLightTargetX2	=17
const kLightTargetX3	=18
const kLightTargetX4	=19
const kLightSJP			=20
const kLightSwitch		=21
const kLightOrbitLeft2	=22				' Partial Left Orbit
const kLightOrbitRight2	=23				' Partial Right Orbit
const kLightBumper		=24
const kLightUnderworld	=25
const kLightPowerSpinners	=26
const kLightPowerPops		=27
const kLightPowerRamps		=28
const kLightPowerTargets	=29
const kLightPowerOrbits		=30
const kLightLoopLeft2		=31			' Full Loop
const kLightSlingLeft		=32
const kLightSlingRight		=33
const kLightLoopRight2		=34			' Full Loop


const kMaxLights		= 35			' Indexes are 0 based so max is +1

dim LightMap()						' Mapping of real lights to indexes
Sub SetupLightMap()
	Redim LightMap(kMaxLights)
	set LightMap(kLightRampLeft)=lRampLeft
	set LightMap(kLightRampRight)=lRampRight
	set LightMap(kLightRampCenter)=lRampCenter
	set LightMap(kLightLoopLeft)=lLoopLeft
	set LightMap(kLightLoopRight)=lLoopRight
	set LightMap(kLightOrbitLeft)=lOrbitLeft
	set LightMap(kLightOrbitRight)=lOrbitRight
	set LightMap(kLightSpinnerLeft)=lSpinnerLeft
	set LightMap(kLightSpinnerRight)=lSpinnerRight		' No Real Light for this
	set LightMap(kLightSpinnerStop)=lSpinnerStop		' No Real Light for this
	set LightMap(kLightSpinnerStopL)=lSpinnerStopL		' No Real Light for this
	set LightMap(kLightPharaohTarget)=lPharaohTarget		' No Real Light for this
	set LightMap(kLightCaptiveBall)=lCaptiveBall
	set LightMap(kLightLock)=lLightLock
	set LightMap(kLightOrb)=lLightOrb
	set LightMap(kLightBonusX)=lBonusX
	set LightMap(kLightTargetX1)=lTargetX1
	set LightMap(kLightTargetX2)=lTargetX2
	set LightMap(kLightTargetX3)=lTargetX3
	set LightMap(kLightTargetX4)=lTargetX4
	set LightMap(kLightSJP)=lJackpot
	set LightMap(kLightSwitch)=lSwitch					' No Real Light for this
	set LightMap(kLightOrbitLeft2)=lLightOrbitLeft2
	set LightMap(kLightOrbitRight2)=lLightOrbitRight2
	set LightMap(kLightBumper)=lBumper
	set LightMap(kLightUnderworld)=lLightUnderworld		' No real light for this
	set LightMap(kLightPowerSpinners)=lPowerSpinner
	set LightMap(kLightPowerPops)=lPowerPops
	set LightMap(kLightPowerRamps)=lPowerRamps
	set LightMap(kLightPowerTargets)=lPowerTargets
	set LightMap(kLightPowerOrbits)=lPowerOrbits
	set LightMap(kLightLoopLeft2)=lLightLoopLeft2			' No Real Light for this 
	set LightMap(kLightLoopRight2)=lLightLoopRight2			' No Real Light for this 
	set LightMap(kLightSlingLeft)=lLightSlingLeft			' No Real Light for this 
	set LightMap(kLightSlingRight)=lLightSlingRight			' No Real Light for this 
End Sub 
SetupLightMap

' Define a variable for all the modes in order of presidence (Higher modes take presidence over lower) 
Const kModeEddie	=0
Const kMode2M2M     =1
Const kModeIcarus	=2
Const kModeHallowed	=3
Const kModeRime		=4
Const kModeFear		=5
Const kModeAces		=6
Const kModeMummy    =7
Const kModeCyborg   =8
Const kModeTrooper  =9
Const kModeRTTH  	=10
Const kModeMadness  =11
Const kModeNOTB  	=12
Const kModeMISC  	=13				' Misc modes like Tomb Treasure 

const kMAX_MODES		= 14		' Total Modes

Dim ModeLightState(4, 14, 35)			'  MaxPlayers, kMAX_MODES, kMaxLights
Dim ModeLightColor(4, 14, 35)			'  MaxPlayers, kMAX_MODES, kMaxLights
Dim StackedLights(4, 14, 35)			'  MaxPlayers, kMAX_MODES, kMaxLights
Dim StackedLightsIdx(35)				'  MaxPlayers, kMAX_MODES, kMaxLights

Sub SetLightColorRestore(lIndex)
	Dim finalColor
	Dim finalState 
	Dim i
	Dim LightStackCount

	finalState = 0
	if lIndex <= kLightSpinnerLeft then ' RGB Lights 
		finalColor = white
	Else 
		finalColor = -1					' Dont set color by default for non RGB
	End if 

	LightStackCount=0
	for i = 0 to kMAX_MODES-1					' Go through all Modes
		StackedLights(CurrentPlayer, i, lIndex)=-1
		if IsModeActive(i) then										' Mode Active 
			if ModeLightState(CurrentPlayer, i, lIndex)<>0 then

				' Handle Stacked Cycling Lights (Skip Eddie)
'				If i <> 0 then 
					StackedLights(CurrentPlayer, i, lIndex)=1
					if StackedLightsIdx(lIndex) = -1 then 
						StackedLightsIdx(lIndex)=i
					End if 
					LightStackCount=LightStackCount+1
'				End if 

				' Handle Primary Light
				finalState = ModeLightState(CurrentPlayer, i, lIndex)
				finalColor = ModeLightColor(CurrentPlayer, i, lIndex)
'WriteToLog "     ", "ModeIndex: " & i & " lightIdx:" & lIndex & " state:" & finalState & " color:" & finalColor
			End If				
		End if 
	Next
	if LightStackCount<=1 then StackedLightsIdx(lIndex)=-1	' Disable stacked 

WriteToLog "     ", "SetLightColorRestore " & lIndex & " state:" & finalState & " color:" & finalColor
'WriteToLog "     ", "SetLightColor:" & LightMap(lIndex).name & " Idx:" & lIndex &","&finalState
	SetLightColor LightMap(lIndex), finalColor, finalState
End Sub 

Sub SSetLightColor(modeIdx, lIndex, color, state)
WriteToLog "     ", "SSetLightColor:" & modeIdx & " " & lIndex & " " & color & " " & state

	ModeLightState(CurrentPlayer, modeIdx, lIndex)=state
	ModeLightColor(CurrentPlayer, modeIdx, lIndex)=color
	SetLightColorRestore lIndex
End Sub

Sub CycleStackedLights()	' This will cycle colors on all the stacked lights
	Dim i
	Dim j
	Dim bModesStacked:bModesStacked=False 

	if bGameInPlay=False then Exit Sub 

	For j = 0 to kMaxLights-1			' See if any lights are on
		if StackedLightsIdx(j)<>-1 then
			if RainbowMode=-1 or (RainbowMode<>-1 and MyRainboxIdx=j) then ' Skip if we are in rainbow Mode 
'WriteToLog "     ", "LightStacked Idx:" & j
'WriteToLog "     ", "Current Stacked Mode:" & StackedLightsIdx(j)
				for i = 0 to kMAX_MODES-1					' Go through all Modes
					StackedLightsIdx(j)=StackedLightsIdx(j)+1
					if StackedLightsIdx(j)>=kMAX_MODES then StackedLightsIdx(j)=0
					if StackedLights(CurrentPlayer, StackedLightsIdx(j), j)<>-1 then 
'WriteToLog "     ", "Changing Stacked Idx:" & StackedLights(CurrentPlayer, i, j)
						SetLightColor LightMap(j), ModeLightColor(CurrentPlayer, StackedLightsIdx(j), j), -1
						Exit For
					End if 
				Next 
			End if 
		End if 
	Next

End Sub 

Sub tmrStackedLights_Timer()
	CycleStackedLights		' Do this at a lower rate
End Sub 

Function GetLightState(modeIdx, lIndex)
	GetLightState=ModeLightState(CurrentPlayer, modeIdx, lIndex)
End Function
Function GetLightColor(modeIdx, lIndex)
	GetLightColor=ModeLightColor(CurrentPlayer, modeIdx, lIndex)
End Function




Sub SetModeQual(modeIdx, bValue)
	ModeQual(CurrentPlayer, modeIdx)=bValue
	ScorbitBuildGameModes
End Sub 

Function IsModeQual(modeIdx)
	IsModeQual=ModeQual(CurrentPlayer, modeIdx)
End Function
 

Sub SetModeActive(modeIdx, bValue)
	ModeActive(CurrentPlayer, modeIdx)=bValue
	ScorbitBuildGameModes
End Sub 

'Function GetActiveMode()	' Returns the current mode or -1 if not mode is active
'	Dim i
'	for i = 0 to kMAX_MODES					' Go through all Modes
'		if IsModeActive(i) then	
'			GetActiveMode=i
'			Exit Function
'		End if 
'	Next 
'	GetActiveMode=-1
'End Function

Function IsModeActive(modeIdx)
	IsModeActive=ModeActive(CurrentPlayer, modeIdx)
End Function

Function GetActiveMode()					' Gets the base mode that is started 
	Dim i
	for i =kMode2M2M to kModeAces			' See which mode is lit 
		if IsModeActive(i) then
			GetActiveMode=i
			exit Function 
		End if 
	Next
	GetActiveMode=-1
End Function 

Function GetActiveModeAll()					' Gets Any Mode that is running except Misc & Eddie 
	Dim i
	for i =kMode2M2M to kModeNOTB			' See which mode is lit 
		if i=kModeTrooper or i=kModeMummy or i=kModeNOTB or i=kModeCyborg then 				' These mode must be qual
			if IsModeQual(i) then 
				GetActiveModeAll=i
				exit Function 
			End if 
		elseif IsModeActive(i) then	
			GetActiveModeAll=i
			exit Function 
		End if 
	Next
	GetActiveModeAll=-1
End Function 

Function GetCurrentMode()					' Only get the base mode that is ready to start
	Dim i
	for i =kMode2M2M to kModeAces			' See which mode is lit 
		if Mode(CurrentPlayer, i)=2 then
			GetCurrentMode=i
			exit Function 
		End if 
	Next
	GetCurrentMode=-1
End Function 

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' Save Player States
Sub SaveCurrentPlayer()
	bGuardianOpen(CurrentPlayer)=(FlashGuardianState=2) 
	saveSong(CurrentPlayer)=Songnr
End Sub 

' Restore Player States
Sub RestoreCurrentPlayer()		' Needs to be merged with ResetForNewPlayerBall
	dim i
	For i =0 to kMaxLights-1
		SetLightColorRestore i
	Next 

	ResetPFxTargetLights		' Have to do this after clearing all lights in the loop above

	Songnr=saveSong(CurrentPlayer)

	FlashGuardianState=0													' Set up Tomb Treasures
	if bTombTreasureReady(CurrentPlayer) then FlashGuardianState=2

	OpenRamp2 bGuardianOpen(CurrentPlayer), True 

	if MysteryLevel(CurrentPlayer)<>0 then
		lOrbArrow.State = 2
	End if 

	light003.state=0
	light004.state=0
	if REVIVELActive(CurrentPlayer) then light004.state=1
	if REVIVERActive(CurrentPlayer) then light003.state=1

	if Score(CurrentPlayer)=0 then 	' Their 1st ball
		' TBD
	End if 

	' Setup Trooper Locks
	Light007.state=0
	Light006.state=0
	if TrooperLocks(CurrentPlayer)>=1 then Light007.state = 1
	if TrooperLocks(CurrentPlayer)=2 then Light006.state = 1

	SetupEddieInserts
	ResetDrop

	' Restore Cyborg 
	If IsModeActive(kModeCyborg) and IsModeQual(kModeCyborg)=False then 
		EnableCyborg

	ElseIf IsModeActive(kModeCyborg)=False and IsModeQual(kModeCyborg)=False then 	' Drained while Cyborg was ready

		If(bPowerPops2(CurrentPlayer)=3)AND(bPowerSpinners2(CurrentPlayer)=3)AND(bPowerRamps2(CurrentPlayer)=3)AND(bPowerTargets2(CurrentPlayer)=3)AND(bPowerOrbits2(CurrentPlayer)=3)Then
			EnableCyborg
		End If
	End if 

End Sub 

' droptargets, animations, etc
Sub VPObjects_Init
    post001_IsDropped(1)
    post002_IsDropped(1)
End Sub

Sub post001_IsDropped(bDropped)
	post001.IsDropped=bDropped
	if bDropped then 
		primPost001.z=-100
	Else 
		PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
		primPost001.z=0
	End if 
End Sub 
Sub post002_IsDropped(bDropped)
	post002.IsDropped=bDropped
	if bDropped then 
		primPost002.z=-100
	Else 
		primPost002.z=0
	End if 
End Sub 


Dim ModePoints(4, 14) '4 players, 14 posible modes  kMAX_MODES
Dim ModeActive(4, 14) '4 players, 14 posible modes  kMAX_MODES
Dim ModeQual  (4, 14) '4 players, 14 posible modes  kMAX_MODES		' Some Modes can be active but not qualified yet
Dim Mode      (4, 14) '4 players, 14 posible modes		kMAX_MODES		' Stores the light states/mode progress
Dim ModeJPPoints(4, 14) '4 players, 14 posible modes  kMAX_MODES
Dim ModeSJPPoints(4, 14) '4 players, 14 posible modes  kMAX_MODES
Dim ModeWaitPlayfieldQual(4, 14)	'4 players, 14 posible modes  kMAX_MODES

Dim bActiveModeDisabled
Dim BullseyeMultiplier		' 1=Outer Ring, 2=Middle Ring, 3=Center Bullseye
Dim EDDIELetter(4)
Dim EDDIELetterStart		' First One 
Dim EddieCard(4, 4)			'4 players, 4 posible cards
Dim EddieRndIdx(4, 3)		' Random Inserts for Last Eddie Phase 
'Dim NewMode
Dim bBallAddedThisMode
Dim bFirstPlay				' First time play the full intro video with sound 

Dim bMummySJP
Dim bMummySJPCnt(4)			' How Many SJPs have you done
Dim bMummyLetterJP
Dim MummyMBProgress(4)		' Overall MummyMB SJP Progress
Dim MummySaveReady(4)		' Save that MummyWas ready so we can go back to it
Dim MummySwitchHitsDoubled(4)
Dim MummySwitchHits(4)
Dim MummyLockReady(4)
Dim MummySwitchHitsMax(4)
Dim MummyTogglePos(4)		' After we start mummy 2 times we need to get 2 hits
Dim MummyStartCnt(4)		' How many times have we started mummy 
Dim MummyTimes(4)			' How many rounds of MummyMB have you done (Resets at the end of MummyMB)
Dim MummyCount(4)			' MUMMY Letter Progres
Dim MummyAddBallMode(4)		' 3 Add a ball modes to mummy
Dim MummyAddBallCnt(4)		' Only get 3 add-a-balls for mummyMb
Dim MummyScepterCnt(4)		' Number of Scepter Shots 
Dim DropCount
Dim bDropSweep
Dim DropPos(4)
Dim DropValue(4)
Dim DropBankValue(4)
Dim MysteryLevel(4)
Dim BallsLocked(4)
Dim bLockActive(4)
Dim bTrooperMBJackpotReady
Dim bTrooperMBFirstHit		' First hit can add a ball
Dim TrooperLocks(4)			' Number of Trooper Locks 
Dim bTrooperCannonReady		' Is Cannon shot available to start 
Dim TrooperMBMultiplier(4, 8)
Dim TrooperMBStartedCount(4)' How many time have we started trooper

Dim PowerSpinnersCount(4)	' Current Progress
Dim PowerPopsCount(4)
Dim PowerRampsCount(4)
Dim PowerTargetsCount(4)
Dim PowerOrbitsCount(4)
Dim bPowerSpinners2(4)		'0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
Dim bPowerPops(4)			' Progress Count 
Dim bPowerPops2(4)			'0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
Dim bPowerRamps2(4)			'0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
Dim bPowerTargets2(4)		'0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
Dim bPowerOrbits2(4)		'0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
Dim PowerFeatureValue
Dim PowerJackpotMultiplier
Dim bSPJActivated
Dim bPJActivated 'ANDREW
Dim CyborgSJPValue(4)
Dim bCyborgSJPActivated
Dim CyborgSJPMultiplier
Dim CyborgSJPCount
Dim CyborgDifficulty(4)
Dim CyborgSaveStart
Dim PFxCount
Dim PFxActivated
Dim AcesHighCount
Dim AcesHighCompletions 	' How many cycles have we done in the current Aces Mode
Dim FearOfTheDarkCount
Dim FearOfTheDarkSpinActive
Dim FearOfTheDarkTimerCount
Dim FearOfTheDarkSpinnerValue
Dim MarinerCount
Dim MarinerProgress
Dim MarinerDifficulty		' How many shots are lit during Mariner
Dim MarinerJPValue
Dim MarinerJPValueStart
Dim MarinerJPMult
Dim MarinerSJPCount
Dim MarinerStep
Dim NOTBPhase ' Andrew Added
Dim NOTBAttackCount ' Andrew Added
Dim NOTBYellowAttackCount ' Andrew Added
Dim NOTBPhase2HitCount ' Andrew Added
Dim NOTBCurrentLightOut 'Andrew Added
Dim BeastShardPoints 'Andrew Added
Dim AcesShardPoints 'Andrew Added
Dim FearShardPoints 'Andrew Added
Dim MarinerShardPoints 'Andrew Added
Dim HallowedShardPoints 'Andrew Added
Dim IcarusShardPoints 'Andrew Added
Dim HallowedCount
Dim HallowedShots(4)	' Store Hallowed progress
Dim HallowedTimerCount
Dim Trigger008used 'left ramp
Dim Trigger009used 'right ramp
Dim Trigger005used 'right orbit
Dim Trigger007used 'left inside orbit
Dim Trigger012used ' right inside orbit
Dim Target004used 'left 
Dim Target005used 'centre
Dim Target006used 'right
Dim Target009used 'captive ball
Dim IcarusValue
Dim IcarusMultiplier
Dim IcarusTimerCount
Dim TwoMinutesToMidnightHitCount
Dim TwoMinToMidnightTimerCount
Dim bTwoMinToMidnightCompleted(4)
'Dim LoopValue
Dim LoopJackpot(4)				' Holds Current LOOP JP Score 
Dim LoopJackpotMulti(4)			' Holds how many loops we have completed 
Dim LoopJackpotQual(4)			' Holds index to calculate number of loops needed
Dim LoopCount					' Hold how many Loop Lights we have done for scoring 
Dim LoopJPCount					' Hold how many Loop Jackpots we have collected 
Dim Loop2x
Dim LoopExtraBallCount(4)
Dim MiniLoopMulti
Dim REVIVEProgress(4) 	' TrackProgress for spelling REVIVE 
Dim REVIVECount(4) 		' Numer of times we completed Revive 
Dim REVIVECountDown(4)	' Countdown to Revive

Dim REVIVELActive(4) ' Lit REVIVEs carry over between balls per lane - ANDREW
Dim REVIVERActive(4) ' Lit REVIVEs carry over between balls per lane- ANDREW
Dim R1(4) 'REVIVE Letter - ANDREW
Dim E1(4) 'REVIVE Letter - ANDREW
Dim V1(4) 'REVIVE Letter - ANDREW
Dim I1(4) 'REVIVE Letter - ANDREW
Dim V2(4) 'REVIVE Letter - ANDREW
Dim E2(4) 'REVIVE Letter - ANDREW
Dim bComboActivated
Dim Combo(6)
Dim ComboCompletions(4)		' Total Combos maxed at 6
Dim ComboTotal(4)			' Total Combos
Dim ComboCount
Dim ComboPopCount			' You can hit 1 pop during combo 
Dim ComboValue
Dim bSuperSlings
Dim bSuperCombo
Dim SwitchBonusCount 'to bonus
Dim SpinnerBonusCount ' Added spinner
Dim RampBonusCount ' Added Ramp
Dim PopsBonusCount ' Added Pops
Dim TargetBonusCount 'Added Targets
Dim PowerFeatureBonusCount ' Andrew
Dim SoulShardCount(4)	' How Many Shards we collected
Dim SoulShardTotal(4, 6)	' Total Collected for the 6 Soul Shard Modes 
Dim EDDIESCollectedBonusCount' Andrew/Chris
Dim MummyBonusCount
Dim DeathblowBonusCount
Dim LoopsBonusCount
Dim TwoMinutesToMidnightBonusCount
Dim TwoMinutesToMidnightBonusMult
Dim SlingshotValue
Dim TombTreasureCount(4)
Dim TombTreasureObj(4, 11)			' 4 players, 11 Treasures (Values:0 - Not Awarded, 1-Awarded, 2-Collected)
Dim bTombTreasureReady(4)
Dim bGuardianOpen(4)
Dim bMummyDisabled
Dim saveSong(4)
Dim LeftTombHits(4)
Dim LeftTombHits2(4)
Dim RightTombHits(4)
Dim CanIPlayWithMadnessStep
Dim bRunToTheHillsSJPEnabled
Dim RTTH_SJP						' Total SJP
Dim RTTH_JPX						' Jackpot Multiplier

Sub Game_Init() 'called at the start of a new game
    Dim i, j, k
    bExtraBallWonThisBall = False
	bCreatedBall=False
    TurnOffPlayfieldLights()
    'Play some Music

    'Init Variables
    For i = 0 to 4
        For j = 0 to kMAX_MODES-1
            Mode(i, j) = 0
			ModeActive(i,j)=False
			ModeQual(i,j)=False 
			ModePoints(i,j)=0
			ModeJPPoints(i,j)=0
			ModeSJPPoints(i,j)=0
			ModeWaitPlayfieldQual(i,j)=False
			StackedLightsIdx(j)=-1
			if j<4 then EddieCard(i, j)=0
			if j<3 then EddieRndIdx(i, j)=-1
			for k=0 to kMaxLights-1
				ModeLightState(i, j, k)=0
				ModeLightColor(i, j, k)=-1
				StackedLights(i, j, k)=0
			Next 
        Next
		ModeActive(i, kModeMISC)=True					' Always have these active
		ModeActive(i, kModeEddie)=True
    Next
    For i = 0 to 4
		Mode(i, INT(RND*5)+2)=2 				'Random Default Mode 
		bPowerFeatureEBCollected(i)=False 
        Jackpot(i) = 1000000
        SuperJackpot(i) = 5000000
        SkillshotValue(i) = 1000000
        SuperSkillshotValue(i) = 5000000
        EDDIELetter(i) = 2
		MummyStartCnt(i) = 0
		MummyTogglePos(i)=True 
        MummyTimes(i) = 0
		bMummySJPCnt(i)=0
		MummyMBProgress(i)=0
        MummyCount(i) = 2
		MummyAddBallMode(i)=0
		MummyAddBallCnt(i)=0
		MummySaveReady(i)=False
		MummyScepterCnt(i)=0
		MummySwitchHits(i) = 0
		MummyLockReady(i)=False
		MummySwitchHitsMax(i)=10
		MummySwitchHitsDoubled(i)=False 

		REVIVECount(i)=0
		REVIVECountDown(i) = 10
		REVIVEProgress(i)= DMDFet(kDMDFet_ReviveStartCnt)		' REV
		REVIVELActive(i) = False
		REVIVERActive(i) = False
		light003.state=0
		light004.state=0

		R1(i) = -1 'REVIVE Letter - ANDREW
		E1(i) = -1 'REVIVE Letter - ANDREW
		V1(i) = -1 'REVIVE Letter - ANDREW
		I1(i) = 10 'REVIVE Letter - ANDREW
		V2(i) = 10 'REVIVE Letter - ANDREW
		E2(i) = 10 'REVIVE Letter - ANDREW
        BallsLocked(i) = 0
        bLockActive(i) = False
        DropPos(i) = 1
        PowerSpinnersCount(i) = 0
        PowerPopsCount(i) = 0
        PowerRampsCount(i) = 0
        PowerTargetsCount(i) = 0
        PowerOrbitsCount(i) = 0
        bPowerPops(i) = 0
        bPowerSpinners2(i) 	= 0 '0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
        bPowerPops2(i) 		= 0 '0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
        bPowerRamps2(i) 	= 0 '0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
        bPowerTargets2(i) 	= 0 '0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
        bPowerOrbits2(i) 	= 0 '0=Not Started, 1=Phase1Done, 2=Phase2Done, 3=Checked
        CyborgSJPValue(i) = 4000000
        LoopExtraBallCount(i) = 0
        LoopJackpot(i) = 0
        DropValue(i) = 50000
        DropBankValue(i) = 250000
        MysteryLevel(i) = 1
        LeftTombHits(i) = 0
        LeftTombHits2(i) = 0
        RightTombHits(i) = 0
		SoulShardCount(i) = 0
		LoopJackpotMulti(i) = 0
		LoopJackpotQual(i) = 0
		CyborgDifficulty(i) = 1

		ComboCompletions(i)=0
		ComboTotal(i)=0

		For j=0to 5
			SoulShardTotal(i, j)=0
		Next 
		TrooperLocks(i)=0
		TombTreasureCount(i) = 0
		For j=0 to 10
			TombTreasureObj(i,j)=0 
		Next 
		bTombTreasureReady(i) = False
		bGuardianOpen(i)=False
		TrooperMBStartedCount(i) = 0
		For j = 0 to 7
			TrooperMBMultiplier(i, j) = 1
		Next
		bTwoMinToMidnightCompleted(i)=False
    Next

	bMultiBallMode=False
	bMummyDisabled=False 
	EDDIELetterStart=True 
	bTableDisabled=False 
	bDropSweep=False
	BallSearchCnt=0
    BullseyeMultiplier = 1
    BallSaverTime = DMDStd(kDMDStd_BallSave)
	bMummyLetterJP=False 	
    bMummySJP = False
	bBallAddedThisMode=False
	bActiveModeDisabled=False
    ResetDrop
	bTrooperCannonReady=False 
    bTrooperMBJackpotReady = False
    bTrooperMBFirstHit = True
    PowerFeatureValue = 0
    PowerJackpotMultiplier = 1
    bSPJActivated = False
	bPJActivated = False
    bCyborgSJPActivated = false
    CyborgSJPMultiplier = 1
    CyborgSJPCount = 0
	CyborgSaveStart=False
    PFxCount = 0
    PFxActivated = 0
'    NewMode = INT(RND * 5) + 4
    AcesHighCount = 0
	AcesHighCompletions=0
    FearOfTheDarkCount = 0
	FearOfTheDarkSpinActive=False 
    FearOfTheDarkSpinnerValue = 0
    MarinerCount = 0
	MarinerProgress=0
	MarinerDifficulty=0
    MarinerJPValue = 1000000
	MarinerJPValueStart=0
    MarinerSJPCount = 0
	MarinerJPMult=10
    MarinerStep = 0
    IcarusValue = 2000000
'    LoopValue = 250000
    LoopCount = 0
	LoopJPCount=0
	Loop2x=False
    MiniLoopMulti = 1
    bComboActivated = False
	tmrDeathBlow.Enabled = False 
    Combo(0) = 0
    Combo(1) = 0
    Combo(2) = 0
    Combo(3) = 0
    Combo(4) = 0
    Combo(5) = 0
    ComboCount = 0
	ComboPopCount=0
    ComboValue = 500000
    bSuperCombo = False
	bSuperSlings = False 
    CanIPlayWithMadnessStep = 0
    bRunToTheHillsSJPEnabled = False
	Trigger008used = 2
	Trigger009used = 2
	Trigger005used = 2
	Trigger007used = 2
	Trigger012used = 2
	Target004used = 2
	Target005used = 2	
	Target006used = 2
	Target009used = 2
	lOrbArrow.State = 2			' Arrow Starts out flashing
	BallsOutlaneDrainCnt=0
	BallsOutlaneDrainIgnoreCnt=0

	RotateRamp2 1, True

	PuPlayer.LabelShowPage pDMDFull,1,0,""
	bFirstPlay=False
	DisableTable False

	RestoreCurrentPlayer

'Init Delays/Timers
'MainMode Init()
'Init lights
End Sub

' https://www.youtube.com/watch?v=p2_jS8GyLSM&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=721s
' https://www.youtube.com/watch?v=p2_jS8GyLSM&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=721s
'Sub InstantInfo
'    Dim tmp
'    DMD CL(0, "INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "JACKPOT VALUE"), CL(1, Jackpot(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "SPR JACKPOT VALUE"), CL(1, SuperJackpot(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "CYBORG JACKPOT"), CL(1, CyborgSJPValue(Currentplayer)), "", eNone, eNone, eNone, 800, False, ""
'    tmp = PowerFeatureValue & " X " & PowerJackpotMultiplier
'    DMD CL(0, "SPR PYRAMID VALUE"), CL(1, tmp), "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "BONUS X"), CL(1, BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "PLAYFIELD X"), CL(1, PlayfieldMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
'    DMD CL(0, "HIGHEST SCORE"), CL(1, HighScoreName(0) & " " & HighScore(0)), "", eNone, eNone, eNone, 800, False, ""
'End Sub


sub StartInstantInfo(keycode)
WriteToLog "     ", "Start Instant " & keycode & " " & bInstantInfo
	if bInstantInfo = False Then ' I am already in instantinfo
		InstantInfoTimer.Interval = 6000
		InstantInfoTimer.Enabled = True
		InstantInfoTimer.UserValue=keycode
	End If 
End Sub 

'Sub InstantInfoTimer_Timer
'    InstantInfoTimer.Enabled = False
'    If NOT hsbModeActive Then
'        bInstantInfo = True
'        DMDFlush
'        InstantInfo
'    End If
'End Sub

Sub InstantInfoTimer_Timer
WriteToLog "     ", "Instant Info timer"

	if bBallInPlungerLane then 
		PuPlayer.LabelShowPage pTransp,2,0,""	' Hide Song Selection
		puPlayer.LabelSet pDMDFull, "SongSelectC", "PupOverlays\\clear.png",1,""
	End if 

    InstantInfoTimer.Enabled = False
    bInstantInfo = True
	bInstantInfoIdx=0
'	PuPlayer.LabelShowPage pDMDFull, 5,0,""
	If DMDFet(kDMDFet_InactivityPause) then PauseTimersForce 10000
	SceneGeneralStartDef True, False, "Backglass", "BG cloud.mp4", "^^^^^^^^^"
	puPlayer.LabelSet pDMDFull,"InstantInfoBG", "PuPOverlays\\txtInfo0.png" 	,1,"{'mt':2,'width':100, 'height':100}"
	puPlayer.LabelSet pDMDFull,"InstantInfoL2",  FormatScore(BonusScoreTotal())	, 1, "{'mt':2,'ypos':45, 'size':10}"

End Sub

Sub StopInstantInfo()
WriteToLog "     ", "StopInstantInfo"
	If DMDFet(kDMDFet_InactivityPause) then PauseTimersForce 500
	InstantInfoTimer.Enabled = False

	if bBallInPlungerLane then 
		PuPlayer.LabelShowPage pTransp,1, 0,""	' Show Song Selection
		ShowSongSelect
		puPlayer.LabelSet pDMDFull, "SongSelectC", "SongSelection\\s" & Songnr  &".png",1,""
	End if 

	bInstantInfo=False 
	bInstantInfoIdx=0
	puPlayer.LabelSet pDMDFull,"InstantInfoBG", "PuPOverlays\\clear.png" ,1,"{'mt':2, 'width':100, 'height':100}"
	puPlayer.LabelSet pDMDFull,"InstantInfoL1", "", 1, ""
	puPlayer.LabelSet pDMDFull,"InstantInfoL2", "", 1, ""
	puPlayer.LabelSet pDMDFull,"InstantInfoL3", "", 1, ""

	puPlayer.LabelSet pDMDFull,"InstantInfoImg1", "PuPOverlays\\clear.png" ,1,""
	puPlayer.LabelSet pDMDFull,"InstantInfoImg2", "PuPOverlays\\clear.png" ,1,""
	puPlayer.LabelSet pDMDFull,"InstantInfoImg3", "PuPOverlays\\clear.png" ,1,""
	puPlayer.LabelSet pDMDFull,"InstantInfoImg4", "PuPOverlays\\clear.png" ,1,""
	puPlayer.LabelSet pDMDFull,"InstantInfoImg5", "PuPOverlays\\clear.png" ,1,""

	QueueStartDefault
End Sub 

Sub EndFlipperStatus(keycode)
	Dim i
	Dim Idx
	Dim AwardsStr
	Dim AwardsStr2
    If bInstantInfo or bGameInPlay=False Then
'WriteToLog "     ", "EndInstantInfo check" & keycode & " " & InstantInfoTimer.UserValue
		if (keycode=InstantInfoTimer.UserValue) then 	' They let go of the key
'WriteToLog "     ", "EndInstantInfo"
			ResetBallSearch
			StopInstantInfo
		Else ' They pressed the other flipper so cycle faster 
			bInstantInfoIdx=bInstantInfoIdx+1
			if bInstantInfoIdx>9 then bInstantInfoIdx=0		' Loop around 
			If DMDFet(kDMDFet_InactivityPause) then PauseTimersForce 10000

			puPlayer.LabelSet pDMDFull,"InstantInfoBG", "PuPOverlays\\txtInfo" & bInstantInfoIdx & ".png" ,1,"{'mt':2,'width':100, 'height':100}"
			Select case bInstantInfoIdx
				case 0:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "TOTAL BONUS ",	 									1, "{'mt':2,'ypos':35, 'size':10}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2",  FormatScore(BonusScoreTotal()), 						1, "{'mt':2,'ypos':45, 'size':10}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", "", 													1, "{'mt':2,'ypos':55, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoImg1", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg2", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg3", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg4", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg5", "PuPOverlays\\clear.png" ,1,""

				case 1:
					puPlayer.LabelSet pDMDFull,"InstantInfoL2",  "",							 						1, "{'mt':2,'ypos':45, 'size':10}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", "", 													1, "{'mt':2,'ypos':55, 'size':8}"

					puPlayer.LabelSet pDMDFull,"InstantInfoImg1", "PuPOverlays\\Shard0.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg2", "PuPOverlays\\Shard0.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg3", "PuPOverlays\\Shard0.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg4", "PuPOverlays\\Shard0.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg5", "PuPOverlays\\Shard0.png" ,1,""

'					' Debug 
'					SoulShardTotal(CurrentPlayer, 0)=10
'					SoulShardTotal(CurrentPlayer, 1)=10
'					SoulShardTotal(CurrentPlayer, 2)=20
'					SoulShardTotal(CurrentPlayer, 3)=3
'					SoulShardTotal(CurrentPlayer, 4)=10

					for i = kModeIcarus to kModeAces
						if SoulShardTotal(CurrentPlayer, i-2)<>0 then puPlayer.LabelSet pDMDFull,"InstantInfoImg" & i-1, "PuPOverlays\\Shard" & i-1 & ".png" ,1,""
					Next 

				case 2:
					puPlayer.LabelSet pDMDFull,"InstantInfoImg1", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg2", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg3", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg4", "PuPOverlays\\clear.png" ,1,""
					puPlayer.LabelSet pDMDFull,"InstantInfoImg5", "PuPOverlays\\clear.png" ,1,""

					' Debug 
'					TombTreasureCount(CurrentPlayer)=9

					AwardsStr=""
					for i = 0 to 9
						if i < TombTreasureCount(CurrentPlayer) then 
							Select case i
								case 0:
									AwardsStr=AwardsStr & "#" & i+1 & ": 15,000,000 Points~"
								case 1:
									AwardsStr=AwardsStr & "#" & i+1 & ": Super Slings~"
								case 2:
									AwardsStr=AwardsStr & "#" & i+1 & ": Can I Play With Madness~"
								case 3:
									AwardsStr=AwardsStr & "#" & i+1 & ": Super Combos~"
								case 4:
									AwardsStr=AwardsStr & "#" & i+1 & ": Light Extra Ball~"
								case 5:
									AwardsStr=AwardsStr & "#" & i+1 & ": Light Revive~"
								case 6:
									AwardsStr=AwardsStr & "#" & i+1 & ": Collect 2X Bonus~"
								case 7:
									AwardsStr2=AwardsStr2 & "#" & i+1 & ": Power Jackpot Increase 5X~"
								case 8:
									AwardsStr2=AwardsStr2 & "#" & i+1 & ": 10M+ L2 Eddie Card~"
								case 9:
									AwardsStr2=AwardsStr2 & "#" & i+1 & ": Run To The Hills~"
							End Select 
						Else 
							if i<7 then 
								AwardsStr=AwardsStr & "#" & i+1 & ": ???~"
							else 
								AwardsStr2=AwardsStr2 & "#" & i+1 & ": ???~"
							End if 
						End if 
					Next
'WriteToLog "     ", AwardsStr
'WriteToLog "     ", AwardsStr2
					puPlayer.LabelSet pDMDFull,"InstantInfoL2",  AwardsStr,										1, "{'mt':2,'xalign':1,'ypos':25, 'size':3}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3",  AwardsStr2,									1, "{'mt':2,'xalign':1,'ypos':58, 'size':3}"
				case 3:
					AwardsStr=""
					If MysteryLevel(CurrentPlayer)=1 then
						AwardsStr=AwardsStr& "LEVEL 1~~"
						AwardsStr=AwardsStr& "Add-A-Ball~"
						AwardsStr=AwardsStr& "More Time~"
						AwardsStr=AwardsStr& "Add Bonus Multiplier~"
						AwardsStr=AwardsStr& "1,000,000~"
						AwardsStr=AwardsStr& "Spot Loop Award~"
						AwardsStr=AwardsStr& "Advance Eddie~"
						AwardsStr=AwardsStr& "Boost Power Jackpot 1,000,000~"
						AwardsStr=AwardsStr& "Advance Mummy"

					elseIf MysteryLevel(CurrentPlayer)=2 then
						AwardsStr=AwardsStr& "LEVEL 2~~"
						AwardsStr=AwardsStr& "Add-A-Ball~"
						AwardsStr=AwardsStr& "More Time~"
						AwardsStr=AwardsStr& "Light Trooper Locks~"
						AwardsStr=AwardsStr& "5,000,000~"
						AwardsStr=AwardsStr& "Advance Revive~"
						AwardsStr=AwardsStr& "Light Eddit Battle Mode ~"
						AwardsStr=AwardsStr& "Boost Power Jackpot 3,000,000"
					else
						AwardsStr=AwardsStr& "LEVEL 3~~"
						AwardsStr=AwardsStr& "Add-A-Ball~"
						AwardsStr=AwardsStr& "More Time~"
						AwardsStr=AwardsStr& "10,000,000~"
						AwardsStr=AwardsStr& "Increase Playfield Multiplier~"
						AwardsStr=AwardsStr& "Start a Power Feature ~"
						AwardsStr=AwardsStr& "Light Revive ~"
						AwardsStr=AwardsStr& "Boost Power Jackpot 6,000,000~"
						AwardsStr=AwardsStr& "Spot Soul Shard"
					End If
					puPlayer.LabelSet pDMDFull,"InstantInfoL2",  AwardsStr,										1, "{'mt':2,'xalign':1,'ypos':25, 'size':3}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3",  "",											1, "{'mt':2,'xalign':1,'ypos':58, 'size':3}"

				case 4:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "REPLAY AT ",	 									1, "{'mt':2,'ypos':35, 'size':10}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2",  FormatScore(ReplayValue), 							1, "{'mt':2,'ypos':50, 'size':10}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", "", 													1, "{'mt':2,'ypos':55, 'size':8}"
				case 5:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "GRAND CHAMPION",								 	1, "{'mt':2,'ypos':35, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2", HighScoreName(0), 									1, "{'mt':2,'ypos':45, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", FormatScore(HighScore(0)),		 					1, "{'mt':2,'ypos':55, 'size':8}"
				case 6:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "HIGH SCORE #1",									 	1, "{'mt':2,'ypos':35, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2", HighScoreName(1), 									1, "{'mt':2,'ypos':45, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", FormatScore(HighScore(1)),		 					1, "{'mt':2,'ypos':55, 'size':8}"
				case 7:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "HIGH SCORE #2",									 	1, "{'mt':2,'ypos':35, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2", HighScoreName(2), 									1, "{'mt':2,'ypos':45, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", FormatScore(HighScore(2)),		 					1, "{'mt':2,'ypos':55, 'size':8}"
				case 8:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "HIGH SCORE #3",									 	1, "{'mt':2,'ypos':35, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2", HighScoreName(3), 									1, "{'mt':2,'ypos':45, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", FormatScore(HighScore(3)),		 					1, "{'mt':2,'ypos':55, 'size':8}"
				case 9:
'					puPlayer.LabelSet pDMDFull,"InstantInfoL1",  "HIGH SCORE #4",									 	1, "{'mt':2,'ypos':35, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL2", HighScoreName(4), 									1, "{'mt':2,'ypos':45, 'size':8}"
					puPlayer.LabelSet pDMDFull,"InstantInfoL3", FormatScore(HighScore(4)),		 					1, "{'mt':2,'ypos':55, 'size':8}"

			End Select
		End If 
	Else
'WriteToLog "     ", "Stop Instant " & keycode
		InstantInfoTimer.Enabled = False
    End If
End Sub


' **********************************
' *****  END INSTANT INFO ROUTINES 
' **********************************


Sub StopMultiballModes 'called at the end of multiball
	Select case GetActiveMode()
		Case kModeIcarus:	'no multi-ball
        Case kModeFear:		'no multi-ball
        Case kModeHallowed:	'no multi-ball
		case kModeRime:StopMariner 1
		case kModeAces:StopAcesHigh 1
	End Select 

	if CyborgSaveStart and bMultiBallMode=False then 		' Start Cyborg back up since it is ready
		CyborgSaveStart=False 
		EnableCyborg
	End if

'    Select case Mode(CurrentPLayer, 0)
'        Case 1:StopMummy:SetupEddieInserts
'        Case 2:StopTrooperMultiball:SetupEddieInserts
'        Case 3:StopCyborg:SetupEddieInserts
'        Case 4: 'no multi-ball
'        Case 5: 'no multi-ball
'        Case 6:StopAcesHigh:ResetEDDIELetter:SetupEddieInserts
'        Case 7: 'no multi-ball
'        Case 8:StopMariner:ResetEDDIELetter:SetupEddieInserts
'        Case 9: 'no multi-ball
'        Case 11:StopCanIPlayWithMadness:SetupEddieInserts
'        Case 12:SetupEddieInserts
'    End Select
End Sub

Dim StopEndOfBallModeEntendTime
Sub StopEndOfBallMode() 'this sub is called after your ball in play is drained, reset skillshot, modes, timers
WriteToLog "     ", "StopEndOfBallMode: " & GetActiveMode()
    Dim tmp
	QueueFlush 0											' EmptyQueue
	RandomVOSoundEndOfBall2									' End of Ball Sound 
'	StopEndOfBallModeEntendTime=0
    If bSkillShotReady Then ResetSkillShot

	bMummyDisabled=False
	EndLoopJP

    Select case GetActiveMode()
        Case kModeIcarus:StopFlightOfIcarus
        Case kModeFear:QueueFlushForce 0, True:StopFearOfTheDark	' Force Flush for Fear because of the way it loops 
        Case kModeHallowed:StopHallowed
		case kModeRime:StopMariner 0
		case kModeAces:StopAcesHigh 0
		case kMode2M2M:StopTwoMinutesToMidnight
    End Select
    PharaohBullseyeFlasherEnabled False
	PharaohBullseyePharoahAward False 

    StopPowerFeature
End Sub

'Sub StopCurrentMode 'called during Tomb awards
'    Select case Mode(CurrentPLayer, 0)
'        Case 1:StopMummy
'        Case 2:StopTrooperMultiball
'        Case 3:StopCyborg
'        Case 4:StopFlightOfIcarus
'        Case 5:StopFearOfTheDark
'        Case 6:StopAcesHigh
'        Case 7:StopHallowed
'        Case 8:StopMariner
'        Case 9:StopTwoMinutesToMidnight
'    End Select
'End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    'Reset the power features if all are completed
    If(bPowerPops2(CurrentPlayer) >=2)AND(bPowerSpinners2(CurrentPlayer) >=2)AND(bPowerRamps2(CurrentPlayer) >=2)AND(bPowerTargets2(CurrentPlayer) >=2)AND(bPowerOrbits2(CurrentPlayer) >=2)Then
'       ResetPowerFeatures		' Dont reset CyborgFinish will do this
    Else
        UpdatePowerFeature
    End If
	UpdateDMDStats
    StopCombo
	UpdateEddieCards
    'bonus counts
    SwitchBonusCount = 0
	SpinnerBonusCount = 0 ' added spinner
	RampBonusCount = 0 ' added ramp
	PopsBonusCount = 0 ' added pops
    MummyBonusCount = 0
	EDDIESCollectedBonusCount=0
    DeathblowBonusCount = 0
    LoopsBonusCount = 0 ' added loops
	TargetBonusCount = 0 'added target 
    PowerJackpotMultiplier = 1
    TwoMinutesToMidnightBonusCount = 0
    SlingshotValue = 330		'10210	- PSM (Keith Elwin Clip)
    bSuperCombo = False
	bSuperSlings = False 
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
WriteToLog "     ", "ResetNewBallLights"
    StopRainbow          ' Set Arrow lights to white and turn them off
    UpdatePFXLights      'ensure the multiplier is displayed right
    UpdateEDDIELetter
    UpdateMummy2		 'ANDREW to update Mummy PF lights
'    SetLightColor lRampCenter, white, 0
    lUnderworld.State = 0
    lBattle.State = 0
    lLock.State = 0
    Light005.State = 2
    RampDown
    ResetPFxTargetLights
    If ExtraBallsLit(CurrentPlayer)>0 then	'light Extra Ball because they were awarded it
        lExtraBall.State = 1
	Else 
		lExtraBall.state = 0
    End If
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
	if bSkillShotReady and bBallInPlungerLane then
		'LightSeqSkillshot.Play SeqAllOff
		Light048.State = 2
		if LFPress then
			bSuperSkillshotReady=True
			bSuperSSkillshotsReady(3)=True 
			lJackpot.State = 2
		Else
			bSuperSkillshotReady=False
			bSuperSSkillshotsReady(3)=False
			lJackpot.State = 0
WriteToLog "     ", "Stopping JP"
		End if 
	End if 
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
WriteToLog "     ", "Skillshot Timer Expired"
    ResetSkillShotTimer.UserValue=ResetSkillShotTimer.UserValue+1
	if ResetSkillShotTimer.UserValue=3 and bSkillShotReady then 		' Standard Skillshot timeout 
		ResetStandardSkillshot
	elseif ResetSkillShotTimer.UserValue >=5 then 
		ResetSkillshot
	End if 
End Sub

Sub ResetStandardSkillshot
	bSkillShotReady = False
    Light048.State = 0
End Sub 

Sub ResetSkillshot
    ResetSkillShotTimer.Enabled = 0
	bSkillShotReady = False
	bSuperSkillshotReady = False 
	bSuperSSkillshotsReady(0) = False 
	bSuperSSkillshotsReady(1) = False 
	bSuperSSkillshotsReady(2) = False 
	bSuperSSkillshotsReady(3) = False 
	bSuperSSkillshotsReady(4) = False 
	bSuperSSkillshotsReady(5) = False 

    'LightSeqSkillshot.StopPlay
    Light048.State = 0
    lJackpot.State = 0
    DMDScoreNow
End Sub 

'*********
' Daphishbowl PupBumper
'*********

Sub tmrPupBumper_Timer 
	dim i
	dim bStopTimer
	bStopTimer=True
	for i = 0 to 8
		if BumperPops(i) <> -1 then
			BumperPops(i) = BumperPops(i) -1
			if BumperPops(i) = 0 then 
				puPlayer.LabelSet pDMDFull,"PopImgM" & i, "PuPOverlays\\Clear.png", 1,""
				puPlayer.LabelSet pDMDFull,"PopImgT" & i, "PuPOverlays\\Clear.png", 1,""
				puPlayer.LabelSet pDMDFull,"PopImgB" & i, "PuPOverlays\\Clear.png", 1,""
				puPlayer.LabelSet pDMDFull,"PopScore" & i, "", 1,""				
				BumperPops(i) =-1
			elseif BumperPops(i) < 8 then	' Show Score and start moving SUPER SLING Text  
				puPlayer.LabelSet pDMDFull,"PopImgT" & i, "PuPOverlays\\Sling2.png", 1,"{'mt':2,'ypos':"&BumperPopsY(i)-(10-BumperPops(i))&",'width':10, 'height':5}"
				puPlayer.LabelSet pDMDFull,"PopImgB" & i, "PuPOverlays\\Sling3.png", 1,"{'mt':2,'ypos':"&BumperPopsY(i)+(10-BumperPops(i))&",'width':10, 'height':5}"
				puPlayer.LabelSet pDMDFull,"PopScore" & i, BumperPopsScore(i)\1000 & "K", 1,""				
				bStopTimer=False
			Elseif BumperPops(i) <= 9 Then	' Change Sling Pic
				puPlayer.LabelSet pDMDFull,"PopImgM" & i, "PuPOverlays\\Sling1.png", 1,"{'mt':2,'width':8, 'height':20}"
				bStopTimer=False
			End If 
		End if
	Next
	if bStopTimer then tmrPupBumper.Enabled = False
End Sub 

dim BumperPopPos
Dim BumperPops(9)
Dim BumperPopsScore(9)
Dim BumperPopsY(9)
BumperPopPos=0
BumperPops(0)=-1:BumperPops(1)=-1:BumperPops(2)=-1:BumperPops(3)=-1:BumperPops(4)=-1:BumperPops(5)=-1:BumperPops(6)=-1:BumperPops(7)=-1:BumperPops(8)=-1:
Sub FlashPupbumper(Score)
	dim x
	Dim y
	if BumperPops(BumperPopPos) = -1 then	' If it is available 
		y=INT(RND*60)+20
		x=INT(RND*70)+15
		BumperPopsScore(BumperPopPos) = score 
		BumperPops(BumperPopPos)=10
		BumperPopsY(BumperPopPos)=y
	'WriteToLog "     ", BumperPopPos & " " & x & " " & y
		puPlayer.LabelSet pDMDFull,"PopImgM" & BumperPopPos, "PuPOverlays\\Sling0.png", 1,"{'mt':2,'color':111111,'ypos':"&y&",'xpos':"&x&",'width':8, 'height':20}"
		puPlayer.LabelSet pDMDFull,"PopImgT" & BumperPopPos, "PuPOverlays\\clear.png", 1,"{'mt':2,'color':111111,'ypos':"&y&",'xpos':"&x&",'width':10, 'height':5}"
		puPlayer.LabelSet pDMDFull,"PopImgB" & BumperPopPos, "PuPOverlays\\clear.png", 1,"{'mt':2,'color':111111,'ypos':"&y&",'xpos':"&x&",'width':10, 'height':5}"
		puPlayer.LabelSet pDMDFull,"PopScore" & BumperPopPos, "", 1,"{'mt':2,'ypos':"&y&",'xpos':"&x&",'ztop':1}"
		tmrPupBumper.Interval = 60
		tmrPupBumper.Enabled = true
	End If 
	BumperPopPos=BumperPopPos+1
	if BumperPopPos>8 then BumperPopPos=0
End Sub 

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'*********************************************************
' Slingshots has been hit

dim SuperSlingScore
Sub CheckSuperSlings()
	if bSuperSlings then 
		FlashPupBumper SuperSlingScore
		AddScore SuperSlingScore
		SuperSlingScore=SuperSlingScore+1000
	End if 
End Sub 


Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        ResetSkillShot
    End If
	RandomSoundSlingshotLeft Lemk
	DOF 103, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore SlingshotValue
	CheckSuperSlings
	ProcessModes(kLightSlingLeft)

    ' add some effect to the table?
'    SetFlash Flasher009, 9, 2000, 50
    ' check modes
    ' remember last trigger hit by the ball
    SetLastSwitchHit "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        ResetSkillShot
    End If
    RandomSoundSlingshotRight Remk
    DOF 105, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore SlingshotValue
	CheckSuperSlings
	ProcessModes(kLightSlingRight)


    ' add some effect to the table?
'    SetFlash Flasher010, 10, 2000, 50
    ' check modes
    ' remember last trigger hit by the ball
    SetLastSwitchHit "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********
' Bumpers
'*********


Dim p,p1:p=125000 'set what P is for the label below, if you change addscore this will need to be changed as well - Chris
Sub Bumper1_Hit
    If Tilted Then Exit Sub

	PauseTimers(-1)
	ProcessModes(kLightBumper)

    RandomSoundBumperTop Bumper1
	DOF 107, DOFPulse
	PopsBonusCount = PopsBonusCount + 1
	'DMD CL(0, "POPS X " & PopsBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
'    Flashforms vLBumper1, 2000, 50, 1 ' Flash for 2 seconds, every 50ms, after turn off
	FlashFlasher1(4)

    ' add some points
    Addscore 50000 + 50000 * bPowerPops2(CurrentPlayer)

    ' check for modes
    StopComboPop
	CheckPowerPops

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Bumper1"
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub

	PauseTimers(-1)
	ProcessModes(kLightBumper)

    RandomSoundBumperMiddle Bumper2
	DOF 111, DOFPulse 
	PopsBonusCount = PopsBonusCount + 1
	'DMD CL(0, "POPS X " & PopsBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    Flashforms LBumper2, 2000, 50, 1
    ' add some points
    Addscore 50000 + 50000 * bPowerPops(CurrentPlayer)
 
   ' check for modes
    StopComboPop
    CheckPowerPops

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Bumper2"
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub

	PauseTimers(-1)
	ProcessModes(kLightBumper)

    RandomSoundBumperBottom Bumper3
	DOF 109, DOFPulse 
	PopsBonusCount = PopsBonusCount + 1
	'DMD CL(0, "POPS X " & PopsBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    Flashforms LBumper3, 2000, 50, 1
    ' add some points
    Addscore 50000 + 50000 * bPowerPops(CurrentPlayer)

	'' This will select the value of P and randomly place it on the screen. - Chris
'	PuPlayer.LabelSet pDMDFull,"Bumper3Txt",FormatNumber(p,0),1,"{'mt':2, 'xpos': " & (15+RndNum(0,50)) & ", 'ypos': " & (20+RndNum(0,60)) & " }" 
'	TriggerScript 600, "PuPlayer.LabelSet pDMDFull,""Bumper3Txt"","""",1,""""" 

    ' check for modes
    StopComboPop
	CheckPowerPops

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Bumper3"
End Sub


Sub Gate001_Hit()			' Update PF when fire the ball
	' Nothing to do here??
End Sub 

'*************************
'      Triggers
'*************************


' ffmpeg -loop 1 -r 30 -s 1920x1080 -i REVIVE_S1.png -vcodec libx264 -crf 25 -b:v 4M -pix_fmt yuv420p -vf scale=1920:1080 -t 4 -profile:v baseline out.mp4
Sub HandleRevive(bLeft)
	puPlayer.LabelSet pDMDFull,"pREVIVE","PuPOverlays\\REVIVE_0.gif",1,"{'mt':2,'color':0,'width':12, 'height':22, 'anigif':100,'pagenum':1}"
'	EnableBallSaver 3
	bBallSaverSkipAnimation=True
	OutlaneBallsaveBuffer = OutlaneBallsaveBuffer+1
	DrainDummy_Hit()

	if bLeft then 
		light004.state = 0
		REVIVELActive(CurrentPlayer) = False
	Else 
		light003.state = 0
		REVIVERActive(CurrentPlayer) = False
	End if 
	PauseTimers 4000
	PlaySoundVol "vo_Revive", VolDef
	QueueScene "PlaySoundVol ""sfx_Rev_C"", VolSfx", 1, 1
	QueueScene "SceneGeneralStartDef False, False, ""Revive"", ""REVIVE_C.mp4"", ""^^^^^^^^^"" ", 4000, 1
	
	If REVIVELActive(CurrentPlayer) = False AND REVIVERActive(CurrentPlayer) = False Then 'Once both lanes have been used and lights are out reset REVIVE
		light003.state = 0
		light004.state = 0
		light005.state = 2
		QueueScene "SceneGeneralStartDef False, False, ""Revive"", ""REVIVE_S.mp4"", ""^^^^^^Next Letter in " & REVIVECountDown(CurrentPlayer) &" spins^^^"" ", 4000, 2
	Else	
		DOF 174, DOFpulse 'Revive Used
	End If

End Sub 


'Sub aSwitches_Hit(idx) 'stop skillshot if any switch is hit
'    'WriteToLog "     ", idx
'    If bSkillShotReady Then
'        ResetSkillShotTimer_Timer
'    End If
'End Sub

Sub Trigger001_Hit 'Left outlane
    If Tilted Then Exit Sub
	DOF 162,DOFpulse
	PlaySoundVol "sfx_outlane", VolSfx
	ProcessModes(kLightSwitch)
    Addscore 100000

    ' check for modes
    StopCombo
    If bSuperSSkillshotsReady(0) and LastSwitchHit="swPlungerRest" Then
        AwardSuperSecretSkillshot 20000000
		IncreaseBallSaver 10
'        AddPlayfieldMultiplier 2
        'BallSaverTime = BallSaverTime + 10
    End If
	If light004.state = 1 AND bBallSaverActive = False Then 'Don't use REVIVE if you still have another Ballsaver Timer running
		HandleRevive(True)
	Else 
		If bBallSaverActive and bTableDisabled=False Then	' Quick add new ball
			DrainDummy_Hit()
		Else 
			if bGameInPlay then RandomVOSoundEndOfBall
		End if 
	End if 

	' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger001"
End Sub

Sub Trigger002_Hit 'Left inlane
    If Tilted Then Exit Sub
	DOF 163, DOFpulse
	' ball is coming in too hot slow it down since trigger in VPX doesnt affect bll motion
	if (ActiveBall.angmomz >100 or ActiveBall.angmomz < -100) then ActiveBall.angmomz=ActiveBall.angmomz/4
	if (ActiveBall.angmomx >100 or ActiveBall.angmomx < -100) then ActiveBall.angmomx=ActiveBall.angmomx/4
'WriteToLog "     ", "ANGMOM:" & ActiveBall.angmomz & " " & ActiveBall.angmomy & " " & ActiveBall.angmomx

	' Check for Shatz
	bShatzEnabled=True
	VpmTimer.AddTimer 1400, "bShatzEnabled=False '"

	PlaySoundVol "sfx_outlane", VolSfx
	ProcessModes(kLightSwitch)
    Addscore 25000
	StartPFMCheck 1
	' check for modes
    If bSuperSSkillshotsReady(1) and LastSwitchHit="swPlungerRest" Then
        AwardSuperSecretSkillshot 5000000
		IncreaseBallSaver 10
'        AddPlayfieldMultiplier 2
        'BallSaverTime = BallSaverTime + 10
    End If
    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger002"
End Sub

Sub Trigger003_Hit 'Right inlane
    If Tilted Then Exit Sub
	DOF 165, DOFPulse
	' ball is coming in too hot slow it down since trigger in VPX doesnt affect bll motion
	if (ActiveBall.angmomz >100 or ActiveBall.angmomz < -100) then ActiveBall.angmomz=ActiveBall.angmomz/4
	if (ActiveBall.angmomx >100 or ActiveBall.angmomx < -100) then ActiveBall.angmomx=ActiveBall.angmomx/4
'WriteToLog "     ", "ANGMOM:" & ActiveBall.angmomz & " " & ActiveBall.angmomy & " " & ActiveBall.angmomx


	PlaySoundVol "sfx_outlane", VolSfx
	ProcessModes(kLightSwitch)
    Addscore 25000
	StartPFMCheck 2
    ' check for modes
    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger003"
End Sub

Sub Trigger004_Hit 'Right outlane
    If Tilted Then Exit Sub
	DOF 164, DOFPulse
	ProcessModes(kLightSwitch)
    Addscore 100000
    ' check for modes
    StopCombo

	If light003.state = 1 AND bBallSaverActive = False Then 'Don't use REVIVE if you still have another Ballsaver Timer running but if theres only 2 seconds left then sue it to avoid ball loss in between timer expiry
		HandleRevive(False)
	Else 
		If bBallSaverActive and bTableDisabled=False Then	' Quick add new ball
			DrainDummy_Hit()
		Else 
			if bGameInPlay then RandomVOSoundEndOfBall
		End if 
	End if 

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger004"
End Sub

Sub Trigger005_Hit 'Start Right Orbit
    If Tilted Then Exit Sub
'    SetFlash Flasher002, 2, 2000, 100

	if LastSwitchHit<>"Trigger006" and bSuperSSkillshotsReady(3) then bSuperSSkillshotsReady(3)=False
	if LastSwitchHit<>"Trigger006" and LastSwitchHit<>"Trigger012" Then DOF 186,DOFpulse 
	ProcessModes(kLightSwitch)
    Addscore 330

	' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger005"
End Sub

Sub Trigger012_Hit 'Mini Loop
    If Tilted Then Exit Sub
	if LastSwitchHit<>"Trigger005" and LastSwitchHit<>"Trigger006" Then DOF 185,DOFpulse 
	ProcessModes(kLightSwitch)
    Addscore 330

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger012"
End Sub

Sub Trigger006_Hit 'Right Orbit, opto (Start counting loops for Pryamids)
    If Tilted Then Exit Sub
    GiEffect 1
    MiniLoopMulti = 1

	if LastSwitchHit<>"Trigger013" and bSuperSSkillshotsReady(3) then bSuperSSkillshotsReady(3)=False

	ProcessModes(kLightSwitch)
    Addscore 330

    'loops, skillshots, addaballs
    LoopsBonusCount = LoopsBonusCount + 1
	If LastSwitchHit = "Trigger005" Then	' kLightOrbitRight
        CheckCombo 5
        If bSuperSSkillshotsReady(5) Then
            AwardSuperSecretSkillshot 8000000
			IncreaseBallSaver 5
'            AddPlayfieldMultiplier 2
            'BallSaverTime = BallSaverTime + 10
        End If
    End If
    If LastSwitchHit = "Trigger012" Then	' kLightLoopRight
        CheckCombo 3
        MiniLoopMulti = 3
    End If
    If LastSwitchHit = "Trigger007" Then	' kLightOrbitLeft
        CheckCombo 2
    End If
    If LastSwitchHit = "Trigger013"  and LastSwitchHit2 <> "swPlungerRest" Then	' kLightLoopLeft2  (No free combo Start)
        CheckCombo 1
    End If

    ' check for modes
	If LastSwitchHit = "Trigger005" then ProcessModes (kLightOrbitRight)
	If LastSwitchHit = "Trigger012" then ProcessModes (kLightLoopRight)
	If LastSwitchHit = "Trigger007" then ProcessModes (kLightOrbitLeft)
	If LastSwitchHit = "Trigger013" and LastSwitchHit2 <> "swPlungerRest" then ProcessModes (kLightLoopLeft2)

	' I saw a case during Multiball where another switch was hit between the 2 for checking Orbit
	if LastSwitchHit="Trigger005" or (bMultiBallMode and LastSwitchHit2="Trigger005") then 
		CheckPowerOrbits
	End if 

    If bSuperSSkillshotsReady(2) and LastSwitchHit="Trigger013" then 
		bSuperSSkillshotOrbits=bSuperSSkillshotOrbits+1

		if bSuperSSkillshotOrbits=3 Then
			AwardSuperSecretSkillshot 20000000
			IncreaseBallSaver 10
'			AddPlayfieldMultiplier 2
		End if 
        'BallSaverTime = BallSaverTime + 10
    End If

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger006"
End Sub

Sub Trigger007_Hit 'Center Orbit, opto
    If Tilted Then Exit Sub
	LoopsBonusCount = LoopsBonusCount + 1
	'DMD CL(0, "LOOPS X " & LoopsBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""

	if LastSwitchHit="Spinner002" and LastSwitchHit2 <> "swPlungerRest" then 	' Make sure we didnt just launch ball
		CheckPowerOrbits
	End if 
	if LastSwitchHit<>"Trigger006" and LastSwitchHit<>"Trigger008" Then DOF 184,DOFpulse
	if LastSwitchHit="Spinner002" then ProcessModes(kLightOrbitLeft2)

	If LastSwitchHit="Trigger006" and LastSwitchHit2="Trigger012" then ProcessModes (kLightLoopRight2)	' Full Loop
	if LastSwitchHit="Trigger006" and LastSwitchHit2="Trigger005" then ProcessModes(kLightOrbitRight2)	' Orbit to Lockball
	ProcessModes(kLightSwitch)
    Addscore 330

	' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger007"
End Sub

Sub Trigger008_Hit 'Left ramp is travelled
    If Tilted Then Exit Sub
	RampBonusCount = RampBonusCount + 1
    'DMD CL(0, "RAMPS X " & RampBonusCount), "", "", eNone, eNone, eNone, 1000, True, "" ' added spinner counting'added ramp counting counting
	DOF 157, DOFpulse
	PlaySoundVol "sfx_ramp3", VolSfx
	ProcessModes(kLightSwitch)
    Addscore 225000
    SetFlash flasher004, 4, 3000, 60
    ' check for modes
    CheckCombo 0		' kLightRampLeft

'    ' Tomb Treasure
'    If bTombTreasureReady = False Then
'        LeftTombHits(CurrentPlayer) = (LeftTombHits(CurrentPlayer) + 1)MOD 5 'each time you hit ramp this counts up by one. once you reach 5 ramps, 5 divided by 5 modulus = 0 then tomb treasure #1 starts
'        If LeftTombHits(CurrentPlayer) = 0 Then SetLightColor lRampLeft, Red, 2 : StartTombTreasure ' lamp getsd overridden by updateeddieletter lamps or whatever mode is going on...may need an if statement in each mode.
'        Else
'			SetLightColor lRampLeft, white, 0 ' lamp gets overridden by updateeddieletter lamps or whatever mode is going on...may need an if statement in each mode
'            AwardTombTreasure
'    End If


'Modes
	CheckPowerRamps 
	ProcessModes (kLightRampLeft)

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger008"
End Sub


Sub Trigger009_Hit 'Right ramp done
    If Tilted Then Exit Sub
	RampBonusCount = RampBonusCount + 1
    'DMD CL(0, "RAMPS X " & RampBonusCount), "", "", eNone, eNone, eNone, 1000, True, "" ' added spinner counting'added ramp counting counting
'    SetFlash flasher005, 5, 3000, 60
	DOF 158, DOFpulse
	PlaySoundVol "sfx_ramp1", VolSfx
	ProcessModes(kLightSwitch)
    Addscore 225000

    ' check for modes
    CheckCombo 4
	ProcessModes (kLightRampRight)
	CheckPowerRamps
	
    ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger009"
End Sub

Sub Trigger013_Hit 'Left Orbit, opto
    If Tilted Then Exit Sub
    Addscore 330
	if LastSwitchHit<>"Trigger008" Then DOF 183,DOFpulse
	if LastSwitchHit<>"swPlungerRest" and bSuperSSkillshotsReady(3) then bSuperSSkillshotsReady(3)=False
	if (LastSwitchHit<>"swPlungerRest" and LastSwitchHit<>"Trigger005") and bSuperSSkillshotsReady(2) then bSuperSSkillshotsReady(2)=False

	If LastSwitchHit="swPlungerRest" then SetLastSwitchHit "Trigger013":exit sub 	' No free progress
	ProcessModes(kLightSwitch)
	ProcessModes(kLightLoopLeft)

 ' remember last trigger hit by the ball
    SetLastSwitchHit "Trigger013"
End Sub

'***************
'   Targets
'***************
Dim bDebounce_Target001:bDebounce_Target001=False
Sub Target001_hit 'skillshot, X
WriteToLog "     ", "Target001_hit"

	if bDebounce_Target001 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target001=True 
	vpmtimer.AddTimer 200, "bDebounce_Target001=False '"

	PlaySoundVol "sfx_ssTarget", VolSfx
	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    CheckDeathblow 1
    Addscore 75000
    If bSkillShotReady and LastSwitchHit="swPlungerRest" Then
        AwardSkillshot
        AddEDDIELetter 1
		IncreaseBallSaver 5
        'BallSaverTime = BallSaverTime + 5
        Exit Sub
    End If

    If lExtraBall.State then 'give Extra Ball
		If LoopExtraBallCount(CurrentPLayer) >=30 then LoopExtraBallCount(CurrentPLayer)=0	' Only reset when if you have 30  loops 
        lExtraBall.State = 0
        AwardExtraBall
    End If

	CheckPFx 1
	ProcessModes(kLightTargetX2)
    CheckPowerTargets

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Target001"
End Sub

Sub Target002_hit 'gravestone
	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub

	PlaySoundVol "sfx_gravestone", VolSfx
    Addscore 75000
    GiEffect 3
'    ' Tomb Treasure
'    If bTombTreasureReady = False Then
'        LeftTombHits2(CurrentPlayer) = (LeftTombHits2(CurrentPlayer) + 1)MOD 5
'        If LeftTombHits2(CurrentPlayer) = 0 Then StartTombTreasure
'    End If
    'Spot an X is comming from the left inlane (alley pass)
    If LastSwitchHit = "Trigger002" and bShatzEnabled then
		if INT(RND * 2)= 0 then 
			PlaySoundVol "vo_hahaAmazing", VolDef
		Else 
			PlaySoundVol "vo_wow2", VolDef
		End if 
		bShatzEnabled=False
		CheckPFx -1 	' Spot a Shot 
    End If

    ' check for modes
    StopCombo
    CheckPowerTargets

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Target002"
End Sub

Dim bDebounce_Target003:bDebounce_Target003=False
Sub Target003_hit 'bumpers, X target
	if bDebounce_Target003 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target003=True 
	vpmtimer.AddTimer 200, "bDebounce_Target003=False '"


	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    Addscore 75000

	CheckPFx 0
	ProcessModes(kLightTargetX1)
    ' check for modes
    StopCombo
    CheckPowerTargets

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Target003"
End Sub

Dim bDebounce_Target004:bDebounce_Target004=False
Sub Target004_Hit 'drop top
WriteToLog "     ", "Target004_Hit"
	if bDebounce_Target004 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target004=True 
	vpmtimer.AddTimer 200, "bDebounce_Target004=False '"

	DTHit 1
	DOF 175, DOFpulse
'	TargetBouncer Activeball, 1
	ShadowDT(0).visible=False

	if target004.uservalue <>0 and target005.uservalue<>0 and target006.uservalue<>0 then 
		WriteToLog "     ", "SWEEP TARGETS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		bDropSweep=True 
	End if 

	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
  ' check for modes
	StopCombo
	ProcessModes(kLightLock)
	if IsModeActive(kModeHallowed)=False or (IsModeActive(kModeHallowed) and HallowedCount<>3) then  ' Skip when hallowed flashing 
		AddScore DropValue(CurrentPLayer)
'        Addscore DropValue(CurrentPLayer)
'        If DropValue(CurrentPLayer) <100000 Then
'            DropValue(CurrentPLayer) = DropValue(CurrentPLayer) + 5000
'        End If
        DropCount = DropCount + 1
        CheckDrops 2
        ' remember last trigger hit by the ball
        SetLastSwitchHit "Target004"
    End If
End Sub

Dim bDebounce_Target005:bDebounce_Target005=False
Sub Target005_Hit()  'drop center
WriteToLog "     ", "Target005_Hit"
	if bDebounce_Target005 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target005=True 
	vpmtimer.AddTimer 200, "bDebounce_Target005=False '"
	
	DTHit 2
	DOF 176,DOFpulse
'	TargetBouncer Activeball, 1
	ShadowDT(1).visible=False

	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    ' check for modes
	StopCombo
	ProcessModes(kLightOrb)
	if IsModeActive(kModeHallowed)=False  or (IsModeActive(kModeHallowed) and HallowedCount<>3) then  ' Skip when hallowed flashing 
		AddScore DropValue(CurrentPLayer)
'        Addscore DropValue(CurrentPLayer)
'        If DropValue(CurrentPLayer) <100000 Then
'            DropValue(CurrentPLayer) = DropValue(CurrentPLayer) + 5000
'        End If
        DropCount = DropCount + 1
        CheckDrops 1
        ' remember last trigger hit by the ball
        SetLastSwitchHit "Target005"
    End If
End Sub

Dim bDebounce_Target006:bDebounce_Target006=False
Sub Target006_Hit()  'drop bottom
WriteToLog "     ", "Target006_Hit"
	if bDebounce_Target006 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target006=True 
	vpmtimer.AddTimer 200, "bDebounce_Target006=False '"

	DTHit 3
	DOF 177,DOFpulse
'	TargetBouncer Activeball, 1
	ShadowDT(2).visible=False

	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    ' check for modes
	StopCombo
	ProcessModes(kLightBonusX)
	if IsModeActive(kModeHallowed)=False  or (IsModeActive(kModeHallowed) and HallowedCount<>3) then  ' Skip when hallowed flashing 
		AddScore DropValue(CurrentPLayer)
'        Addscore DropValue(CurrentPLayer)
'        If DropValue(CurrentPLayer) <100000 Then
'            DropValue(CurrentPLayer) = DropValue(CurrentPLayer) + 5000
'        End If
        DropCount = DropCount + 1
        CheckDrops 0
        ' remember last trigger hit by the ball
        SetLastSwitchHit "Target006"
    End If
End Sub

Dim bDebounce_Target007:bDebounce_Target007=False
Sub Target007_hit 'mini loop target, X

	if bDebounce_Target007 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target007=True 
	vpmtimer.AddTimer 200, "bDebounce_Target007=False '"

	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    Addscore 75000

	CheckPFx 2
	ProcessModes(kLightTargetX3)
    ' check for modes
    StopCombo
    CheckPowerTargets

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Target007"
End Sub

Dim bDebounce_Target008:bDebounce_Target008=False
Sub Target008_hit 'Super Jackpot (Dead End), X, Mushroomcloud

'WriteToLog "     ", "Target008_hit" & bDebounce_Target008 
	if bDebounce_Target008 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_Target008=True 
	vpmtimer.AddTimer 200, "bDebounce_Target008=False '"

	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub
    Addscore 75000
    SetFlash flasher013, 13, 6000, 50
    LightEffect 3

	CheckPFx 3
	ProcessModes(kLightTargetX4)
	ProcessModes(kLightSJP)

    ' check for modes
    CheckDeathblow 5
    If bSuperSkillshotReady Then
        AwardSuperSkillshot
		QualPFM 1
		IncreaseBallSaver 10
        'AddPlayfieldMultiplier 1
        'BallSaverTime = BallSaverTime + 10
        Exit Sub
    End If
    CheckPowerTargets

    ' remember last trigger hit by the ball
    SetLastSwitchHit "Target008"
End Sub

Dim bDebounce_CapWall2:bDebounce_CapWall2=False
Sub CapWall2_Hit()			' Mummy Newton
WriteToLog "     ", "CapWall2_Hit:" & BallsInLock(CurrentPlayer) & " " & BallsInRealLock & " " & RampLock2.Collidable

	if bDebounce_CapWall2 then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_CapWall2=True 
	vpmtimer.AddTimer 200, "bDebounce_CapWall2=False '"


	TargetBonusCount = TargetBonusCount + 1                                                 'added target counting
	'DMD CL(0, "TARGETS X " & TargetBonusCount), "", "", eNone, eNone, eNone, 1000, True, ""
    If Tilted Then Exit Sub

	SoundOnOrbCollision

    SetFlash flasher014, 14, 2000, 60
    SetFlash flasher004, 4, 2000, 60
'    SetFlash flasher005, 5, 2000, 60
    GiEffect 3

	NewtonHit(2)

	' Hit the ball in the lock (if there is one)
	if BallsInLock(CurrentPlayer)>0 then 
		if BallsInRealLock>0 then 
			if RampLock2.Collidable = True then
				CapKicker2a.Kick -8, 45
			else 
				CapKicker2a.Kick -8, 15
			End if
		else 
			' See if we need to add a ball to start multiball 
			if gRampPos=2 then trgBallLock2_Hit
		End if 
	End if 


    ' check for modes
    CheckDeathblow 3
	Addscore 50000    ' 25330
	ProcessModes(kLightCaptiveBall)

    ' remember last trigger hit by the ball
    SetLastSwitchHit "CapWall2"
End Sub 


Sub NewtonHit(idx)		 ' Animate the ball moving
	If idx=1 then 
		tmrNewton1.Interval=10
		tmrNewton1.UserValue=0
		tmrNewton1.Enabled = True 
	Else 
		tmrNewton2.Interval=10
		tmrNewton2.UserValue=0
		tmrNewton2.Enabled = True 
	End if 
End Sub 


Sub tmrNewton1_Timer()
	tmrNewton1.UserValue=tmrNewton1.UserValue+1
	if tmrNewton1.UserValue<=4 then 
		primNBall1.transX=primNBall1.transx+1
		primNBall1.transZ=primNBall1.transz-1
	elseif tmrNewton1.UserValue<=8 then
		primNBall1.transX=primNBall1.transx-1
		primNBall1.transZ=primNBall1.transz+1
	Else 
		primNBall1.transX=0
		primNBall1.transZ=0
		tmrNewton1.Enabled = False
	End if 

End Sub 


Sub tmrNewton2_Timer()
	tmrNewton2.UserValue=tmrNewton2.UserValue+1
	if tmrNewton2.UserValue<=6 then 
		primNBall2.transX=primNBall2.transx+0.25
		primNBall2.transZ=primNBall2.transz-1
	elseif tmrNewton2.UserValue<=12 then
		primNBall2.transX=primNBall2.transx-0.25
		primNBall2.transZ=primNBall2.transz+1
	Else 
		primNBall2.transX=0
		primNBall2.transZ=0
		tmrNewton2.Enabled = False
	End if 

End Sub 


' NOTE you can hit this from the side????
' https://youtu.be/besY8TS0Ges?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=519 
Dim CapWall_bHit:CapWall_bHit=False
Dim bDebounce_CapWall:bDebounce_CapWall=False

Sub CapWall1_Hit 'Maiden ball - assigned to aSwitches 
WriteToLog "     ", "CapWall1_Hit:" & LastSwitchHit & " " & bSkillShotReady
    If Tilted Then Exit Sub

	if bDebounce_CapWall then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce  
	bDebounce_CapWall=True 
	vpmtimer.AddTimer 200, "bDebounce_CapWall=False '"


	If CapWall_bHit=False then 				' First Time 
		SoundOnOrbCollision
		FlashForms ORBLight, 3000, 60, 0

		bFlasher2Enabled=True				' Flash it

		NewtonHit(1)
'		CapKicker1.kick 0, 1				' Make Ball Wiggle
'		CapKicker1.enabled = True 
'		VpmTimer.AddTimer 300, "CapKicker1.enabled = False '"
		VpmTimer.AddTimer 2000, "bFlasher2Enabled=False '"
	End if

    CheckDeathblow 2
	CheckMadnessFreeze

:WriteToLog "     ", "DISABLE SSS4 Orb:" & LastSwitchHit

	If LastSwitchHit="Trigger005" and bSuperSSkillshotsReady(3) Then ' ANDREW - Need to add something like IF LeftFlipper.RotateToEnd = True OR LeftFlipper.State = RotateToEnd (not sure how to check this property?)
		CapWall_bHit=True
        AwardSuperSecretSkillshot 10000000 'full plunge, and one time shot to the orb. 10 Million, plus boost power jackpot
		AddJP cPowerJackpotMultiplier, 1
		IncreaseBallSaver 10
        'BallSaverTime = BallSaverTime + 10
    End If

	' check for modes
	if isModeActive(kModeCyborg)=False then									' No Mystery during Cyborg 
		If lOrbArrow.state=2 and MysteryLevel(CurrentPlayer)>0 then
			CapWall_bHit=True
			AwardMystery
			Exit Sub 				' Award Mystery call back into this 
		End if 
	End if 

	If bPJActivated or bSPJActivated Then
		CapWall_bHit=True
		AwardPJackpot
		Exit Sub 				' Award Mystery call back into this 
	End If

	if bCyborgSJPActivated then 
		CapWall_bHit=True
		AwardCyborgSJP
	End if

	if CapWall_bHit then 
		PlaySoundVol "sfx_ding1", VolSfx
		Addscore 100000							' Should this be 0??
	Else 
		PlaySoundVol "sfx_miss1", VolSfx
		Addscore 25000
	end if 

    SetLastSwitchHit "CapWall1"
	CapWall_bHit=False
End Sub


'	PharaohBullseyeFlasher.Enabled=True:PharaohBullseyeFlasher_Timer		' Flash bulseye, shortcycle initial wait
Dim bPharaohBullseyeFlasher_PharoahAward
Dim bPharaohBullseyeFlasher_Other

Sub PharaohBullseyeFlasherEnabled(bEnabled)
	bPharaohBullseyeFlasher_Other=bEnabled
	PharaohBullseyeFlash bEnabled
End Sub 

Sub PharaohBullseyePharoahAward(bEnabled)
	bPharaohBullseyeFlasher_PharoahAward=bEnabled
	PharaohBullseyeFlash bEnabled
End Sub 

Sub PharaohBullseyeFlash(bEnabled)
	if bEnabled then 
		PharaohBullseyeFlasher.Enabled=True
		PharaohBullseyeFlasher_Timer
	else 
		if bPharaohBullseyeFlasher_Other=False and bPharaohBullseyeFlasher_PharoahAward=False then 
			PharaohBullseyeFlasher.Enabled=False
		End if 

		If bPharaohBullseyeFlasher_PharoahAward=False then 
			lUnderworld.State = 0
			SetDefPulse lUnderworld
		End if 

	End if 
End Sub 

Sub PharaohBullseyeFlasher_Timer
	SetFlash flasher006,   6, 1000, 50
    SetFlash flasher007,   7, 1000, 50
	SetFlash flasher008,   7, 1000, 50 ' Andrew Added
	if bPharaohBullseyeFlasher_Other then 						' Flash Center Shot only if other is also running 
		Flashforms lUnderworld, 1000, 50, 0
		Flashforms lRampCenter, 1000, 50, lRampCenter.State
	else 
		lUnderworld.State = 2
		SetSlowPulse lUnderworld
	End if 
End Sub

Sub AnimateBullseye(color)		' Colors 1=Red, 2=Blue, 3=Purple
	Select Case color
		Case 1:
			flasher006.Color = RGB(255, 0, 0) 	  'RED     RGB(255, 252, 224)  'yellow
			flasher006.opacity=2500
			FlashBeacon "red", True 
		Case 2:
			flasher006.opacity=10000
			flasher006.Color = RGB(0, 0, 255) 	  'BLUE    RGB(255, 128, 0)    ' orange
			FlashBeacon "blue", True 
		Case 3:
			flasher006.opacity=10000
			flasher006.Color = RGB(184, 23, 244)  ' purple
			FlashBeacon "cycle", True 
	End Select
	vpmTimer.AddTimer 4000, "FlashBeacon ""white"", False '"		' Turn it off  after 4 seconds 

	SetFlash FlasherFace, 16, 1400, 50
	SetFlash flasher006,   6, 1400, 50
	SetFlash flasher007,   7, 1400, 50
	SetFlash flasher008,   7, 1400, 50 ' Andrew Added
	Flashforms lUnderworld, 1400, 50, 0
	Flashforms lRampCenter, 1400, 50, lRampCenter.State
End Sub 

Sub pharaoh_target3_Hit()	' Center Bullseye
	BullseyeMultiplier=3
	CheckPharohTargets
End Sub 

Sub pharaoh_target2_Hit()	' Middle Ring
	BullseyeMultiplier=2
	CheckPharohTargets
End Sub 

Sub pharaoh_target_Hit		' Outer Ring
	BullseyeMultiplier=1
	CheckPharohTargets
End Sub 

dim bDebounce_CheckPharohTargets:bDebounce_CheckPharohTargets=False
Sub CheckPharohTargets			' Triggered when you hit any of the bullseye areas at the back 
	Dim JPtxt
	Dim PharoahAwardScore
	DOF 161, DOFpulse	
	DOF 160, DOFpulse	
	DOF 159, DOFpulse

WriteToLog "     ", "CheckPharohTargets:" & BullseyeMultiplier

	if bDebounce_CheckPharohTargets then WriteToLog "     ", "Debounce Skip":exit sub						' Debounce in case 2 targets get hit 
	bDebounce_CheckPharohTargets=True 
	vpmtimer.AddTimer 200, "bDebounce_CheckPharohTargets=False '"

    If Tilted Then Exit Sub
	If bSuperSSkillshotsReady(4) and LastSwitchHit="swPlungerRest" Then
        AwardSuperSecretSkillshot 5000000
		IncreaseBallSaver 5
'        AddPlayfieldMultiplier 2
        'BallSaverTime = BallSaverTime + 5
    End If

	if INT(RND*3)=1 then 
		PlaySoundVol "sfx_gong4", VolSfx		' Looks like a random Gong Sound 
	Else 
		PlaySoundVol "sfx_bullseye2", VolSfx
	End if 

    Addscore 25000 * BullseyeMultiplier
	AnimateBullseye BullseyeMultiplier

'	' This is the PharaohAward (Needs to see when this is really active, seems like when target is flashing)
	if PharaohBullseyeFlasher.Enabled then
		If IsModeActive(kModeNOTB)=False then 	' Not active during NOTB
			PharoahAwardScore=500000
			if BullseyeMultiplier=2 then 
				PharoahAwardScore=1000000
			elseif BullseyeMultiplier=3 then 
				BullseyeMultiplier=4			' Increases to 4x
				PharoahAwardScore=2000000
			End if 
			PharaohBullseyePharoahAward False

			' Add it to the power feature total
			AddJP cPowerFeatureValue, PharoahAwardScore

			AddScore PharoahAwardScore
			JPtxt=FormatScore(PharoahAwardScore)
			if BullseyeMultiplier=1 then 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""JP1.mp4"", ""I:Callouts\\txtJP1.png^^^^^^" & JPtxt & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3034, 1
			else 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""JP1.mp4"", ""I:Callouts\\txtJP1.png^^^^^^500,000 x" & BullseyeMultiplier & "^^" & JPtxt & "^"", ""^^^^^^^^3000:" & pupColorRed & "^"" ", 3034, 1
			End if 
			QueueScene "SceneClearLabels", 0, 1
		End if 
	End if 

' check for modes
	ProcessModes (kLightRampCenter)
    CheckDeathblow 4

    ' remember last trigger hit by the ball
    SetLastSwitchHit "pharaoh_target"
End Sub


'*****************
'    kickers
'*****************

Sub Underworld_Hit
WriteToLog "     ", "Underworld_Hit BallsFromSarcophagus:" & BallsFromSarcophagus & " lBattle.State:" & lBattle.State & " Madness:" & IsModeActive(kModeMadness)

	if BallsFromSarcophagus>0 then 
		BallsFromSarcophagus=BallsFromSarcophagus-1
		vpmtimer.addtimer 1000, "Exit_Underworld False '"
	elseif IsModeActive(kModeMadness) then 
		ProcessModes(kLightUnderworld)
	else 
		StopCombo			' Underworld ends the combo
		SoundSaucerLock
		LightEffect 4
		GiEffect 0
		FlashEffect 4
		DMD "_", "UNDERWORLD!", "", eNone, eBlink, eNone, 1500, True, "" ' Andrew added
		If Tilted Then
			vpmtimer.addtimer 1000, "Exit_Underworld False '"
			Exit Sub
		End If

		RampDown					' Close it
		Addscore 330

	WriteToLog "     ", "Underworld_Hit: " & lBattle.State
		If IsModeActive(kModeCyborg) and IsModeQual(kModeCyborg)=False and GetActiveModeAll() = -1 Then
			StartCyborg False 
		elseIf lBattle.State and IsModeActive(kModeCyborg)=False Then	' Make sure cyborg isnt running 
			StartSongBattleMode
		Else
			' Rime is the only mode that will eject the ball (Handle the Edge just in case something else locks a ball here)
			if IsModeActive(kModeRime)=False then 
				vpmtimer.addtimer 1000, "Exit_Underworld False '"
				WriteToLog "     ", "ERROR - Releasing ball when it shouldnt of got here"
			Else 
				ProcessModes(kLightPharaohTarget)
				ProcessModes(kLightRampCenter)
			End if 
		End If
	End if 
End Sub


Sub Exit_Underworld(bLeaveRampUp)
WriteToLog "     ", "Exit_Underworld: " & bLeaveRampUp
    RampUp 'ensure the ramps is up
    SoundSaucerKick 1, Underworld
	Underworld.Kick 180,65,1.5
    LightEffect 5

	if bLeaveRampUp=True then exit sub 			' Bail out before scheduling ramp to go down
	VpmTimer.AddTimer 800,"RampDown '"
End Sub


'******************************
' MODES: Battles & Multi-balls
'******************************
' Modes(CurrentPlayer), x) = (mode case number)
' x being status
' values 0: not started, 1 finished, 2 ready to start

'1 Mummy MB 
'2 Tooper MB
'3 Cyborg MB - Cyborg - Wizard mode after the Super festures are finished (pops, spinner, ramps...)
'4 Flight of Icarus
'5 Fear of The Dark
'6 Aces High
'7 Hallowed Be Thy Name
'8 Rime of The Ancient Mariner
'9 Two Minutes to Midnight
'10 Spell EDDIE
'11 Can I Play with Madness - 3rd Tomb Award 
'12 Run to the hills - 10th Tomb Award

'********************************
'        Digital clock
'********************************

Dim ClockDigits(4), ClockChars(10)

ClockDigits(0) = Array(a00, a02, a05, a06, a04, a01, a03) 'clock left digit
ClockDigits(1) = Array(a10, a12, a15, a16, a14, a11, a13)
ClockChars(0) = Array(1, 1, 1, 1, 1, 1, 0)                '0
ClockChars(1) = Array(0, 1, 1, 0, 0, 0, 0)                '1
ClockChars(2) = Array(1, 1, 0, 1, 1, 0, 1)                '2
ClockChars(3) = Array(1, 1, 1, 1, 0, 0, 1)                '3
ClockChars(4) = Array(0, 1, 1, 0, 0, 1, 1)                '4
ClockChars(5) = Array(1, 0, 1, 1, 0, 1, 1)                '5
ClockChars(6) = Array(1, 0, 1, 1, 1, 1, 1)                '6
ClockChars(7) = Array(1, 1, 1, 0, 0, 0, 0)                '7
ClockChars(8) = Array(1, 1, 1, 1, 1, 1, 1)                '8
ClockChars(9) = Array(1, 1, 1, 1, 0, 1, 1)                '9

Sub UpdateClock(myTime)
    Dim a, b, i
    a = myTime \ 10
    b = myTime MOD 10
    For i = 0 to 6
        ClockDigits(0)(i).State = ClockChars(a)(i)
        ClockDigits(1)(i).State = ClockChars(b)(i)
    Next
End Sub

Sub TurnOffClock
    Dim i
    For i = 0 to 6
        ClockDigits(0)(i).State = 0
        ClockDigits(1)(i).State = 0
    Next
End Sub
'************************
' Collect EDDIE letters
'************************
' Mode 10, starts as default
' shoot white arrows
' mini loop spots also a letter
' shoot the Underworld Ramp to start the Song battle modes

Sub StopModeLoopingVideos
	pupevent 849	 'STOP ACES vids
	pupevent 669	 'STOP TROOPER vids
	pupevent 709	 'STOP FLIGHT vids
	pupevent 739	 'STOP HALLOWED vids
	pupevent 770	 'STOP MUMMY vids
	pupevent 789	 'STOP FEAR vids
	pupevent 812	 'STOP RIME vids
	pupevent 839	 'STOP TWO MINUTES TO MIDNIGHT vids
	pupevent 879	 'STOP THE MADNESS! vids
	pupevent 929	 'STOP CYBORG vids
	pupevent 909	 'STOP RUN TO THE HILLS vids
	pupevent 979	 'STOP NUMBER OF THE BEAST vids
End Sub


Sub SetupEddieInserts 'called each time to light new white arrows
	Dim rndIdx
	Dim rndIdx2
	Dim rndIdx3
	Dim i
	Dim ModesStarted 

WriteToLog "     ", "SetupEddieInserts:" &  IsModeActive(kModeEddie) & " qual:" &  IsModeQual(kModeEddie)
	If IsModeActive(kModeNOTB) then 
		SSetLightColor kModeNOTB,kLightRampLeft  , White, 0
		SSetLightColor kModeNOTB,kLightLoopLeft  , White, 0
		SSetLightColor kModeNOTB,kLightOrbitLeft , yellow, 2
		SSetLightColor kModeNOTB,kLightRampCenter, White, 0
		SSetLightColor kModeNOTB,kLightLoopRight , White, 0
		SSetLightColor kModeNOTB,kLightRampRight , White, 0
		SSetLightColor kModeNOTB,kLightOrbitRight, yellow, 2

	Elseif IsModeQual(kModeEddie) then 
		SSetLightColor kModeEddie,kLightRampLeft  , White, 0 'arrows off
		SSetLightColor kModeEddie,kLightLoopLeft  , White, 0
		SSetLightColor kModeEddie,kLightOrbitLeft , White, 0
		SSetLightColor kModeEddie,kLightRampCenter, White, 0
		SSetLightColor kModeEddie,kLightLoopRight , White, 0
		SSetLightColor kModeEddie,kLightRampRight , White, 0
		SSetLightColor kModeEddie,kLightOrbitRight, White, 0

		lBattle.State = 2
		if isRampUp()=False then 
			Rampup 'ready to start the mode in the Underworld
		End if 
	Else 

		' See how many modes are done 
		ModesStarted=0
		for i = kModeIcarus to kModeAces
			if Mode(CurrentPlayer, i)=1 then ModesStarted=ModesStarted+1
		Next 
		if bTwoMinToMidnightCompleted(CurrentPlayer) then ModesStarted=8	' Max out if we completed 2m2m

		if ModesStarted<2 Then
			SSetLightColor kModeEddie,kLightRampLeft  , White, 1
			SSetLightColor kModeEddie,kLightLoopLeft  , White, 0	' Never Lit 
			SSetLightColor kModeEddie,kLightOrbitLeft , White, 1
			SSetLightColor kModeEddie,kLightRampCenter, White, 1
			SSetLightColor kModeEddie,kLightLoopRight , White, 1
			SSetLightColor kModeEddie,kLightRampRight , White, 1
			SSetLightColor kModeEddie,kLightOrbitRight, White, 1

		elseif ModesStarted=2 or ModesStarted=3 then 
			SSetLightColor kModeEddie,kLightRampLeft  , White, 1
			SSetLightColor kModeEddie,kLightLoopLeft  , White, 0	' Never Lit 
			SSetLightColor kModeEddie,kLightOrbitLeft , White, 1
			SSetLightColor kModeEddie,kLightRampCenter, White, 0	' Not Lit
			SSetLightColor kModeEddie,kLightLoopRight , White, 1
			SSetLightColor kModeEddie,kLightRampRight , White, 1
			SSetLightColor kModeEddie,kLightOrbitRight, White, 1

		elseif ModesStarted>3 then

			SSetLightColor kModeEddie,kLightRampLeft  , White, 0
			SSetLightColor kModeEddie,kLightLoopLeft  , White, 0	' Never Lit 
			SSetLightColor kModeEddie,kLightOrbitLeft , White, 0
			SSetLightColor kModeEddie,kLightRampCenter, White, 0	' Not Lit
			SSetLightColor kModeEddie,kLightLoopRight , White, 1	' Always lit
			SSetLightColor kModeEddie,kLightRampRight , White, 0
			SSetLightColor kModeEddie,kLightOrbitRight, White, 0
 
		' Pick 2 random to light
			rndIdx=-1:rndIdx2=-1:rndIdx3=-1
			if EddieRndIdx(CurrentPlayer, 0)=-1 then 
				rndIdx=2					' kLightLoopRight
				rndIdx2= INT(Rnd*5)
				while rndIdx = rndIdx2
					rndIdx2=Int(Rnd*5)
				wend
				rndIdx3= INT(Rnd*5)
				while rndIdx3 = rndIdx2 or rndIdx3 = rndIdx
					rndIdx3=Int(Rnd*5)
				wend
				EddieRndIdx(CurrentPlayer, 0)=rndIdx
				EddieRndIdx(CurrentPlayer, 1)=rndIdx2
				EddieRndIdx(CurrentPlayer, 2)=rndIdx3
			else 
				rndIdx=EddieRndIdx(CurrentPlayer, 0)
				rndIdx2=EddieRndIdx(CurrentPlayer, 1)
				rndIdx3=EddieRndIdx(CurrentPlayer, 2)
			End if 
			SSetLightColor kModeEddie,kLightRampLeft  , White, 0
			SSetLightColor kModeEddie,kLightLoopLeft  , White, 0	' Never Lit 
			SSetLightColor kModeEddie,kLightOrbitLeft , White, 0
			SSetLightColor kModeEddie,kLightRampCenter, White, 0	' Not Lit
			SSetLightColor kModeEddie,kLightLoopRight , White, 0	' Always lit
			SSetLightColor kModeEddie,kLightRampRight , White, 0
			SSetLightColor kModeEddie,kLightOrbitRight, White, 0

			if rndIdx=0 or rndIdx2=0 or rndIdx3=0 then SSetLightColor kModeEddie,kLightRampLeft  , White, 1
			if rndIdx=1 or rndIdx2=1 or rndIdx3=1 then SSetLightColor kModeEddie,kLightOrbitLeft  , White, 1
			if rndIdx=2 or rndIdx2=2 or rndIdx3=2 then SSetLightColor kModeEddie,kLightLoopRight , White, 1
			if rndIdx=3 or rndIdx2=3 or rndIdx3=3 then SSetLightColor kModeEddie,kLightRampRight , White, 1
			if rndIdx=4 or rndIdx2=4 or rndIdx3=4 then SSetLightColor kModeEddie,kLightOrbitRight, White, 1

		End if

		WriteToLog "     ", "SetupEddieInserts: ModesStarted:" & ModesStarted & " " & rndIdx & " " & rndIdx2 & " " & rndIdx3

'		if ModesStarted>=1 then 
'			if ModesStarted>=3 then 
'				rndIdx=kLightLoopRight	' This is always lit on the hardest
'			Else 
'				rndIdx = INT(RND*6)
'			End if 
'		End if 
'		if ModesStarted>=2 then 
'			rndIdx2= INT(Rnd*6)
'			while rndIdx = rndIdx2
'				rndIdx2=Int(Rnd*6)
'			wend
'		End if 
'		if ModesStarted>=3 then 
'			rndIdx3= INT(Rnd*6)
'			while rndIdx3 = rndIdx2 or rndIdx3 = rndIdx
'				rndIdx3=Int(Rnd*6)
'			wend
'		End if 
'
'		if rndIdx=0 or rndIdx2=0 or rndIdx3=0 then SSetLightColor kModeEddie,kLightRampLeft  , White, 0
'		if rndIdx=1 or rndIdx2=1 or rndIdx3=1 then SSetLightColor kModeEddie,kLightOrbitLeft  , White, 0
'	'	if rndIdx=2 or rndIdx2=2 or rndIdx3=2 then SSetLightColor kModeEddie,kLightLoopLeft , White, 0
'		if rndIdx=2 or rndIdx2=2 or rndIdx3=2 then SSetLightColor kModeEddie,kLightRampCenter, White, 0
'		if rndIdx=3 or rndIdx2=3 or rndIdx3=3 then SSetLightColor kModeEddie,kLightLoopRight , White, 0
'		if rndIdx=4 or rndIdx2=4 or rndIdx3=4 then SSetLightColor kModeEddie,kLightRampRight , White, 0
'		if rndIdx=5 or rndIdx2=5 or rndIdx3=5 then SSetLightColor kModeEddie,kLightOrbitRight, White, 0


	End if 
End Sub


Sub AddEDDIELetter(n)
    ' if not at the maximum letters
    if(EDDIELetter(CurrentPlayer) < 5)then
WriteToLog "     ", "AddEDDIELetter"

		If EDDIELetterStart then
			if INT(RND*2)=0 then 
				PlaySoundVol "vo_spelleddie", VolDef
			Else 
				PlaySoundVol "vo_shootwhitearrowstosummoneddie", VolDef
			End If 
		Else 
			PlaySoundVol "vo_greatshot", VolDef
		End If 
		EDDIELetterStart=False

        ' then add and set the lights
        EDDIELetter(CurrentPlayer) = EDDIELetter(CurrentPlayer) + n
		if EDDIELetter(CurrentPlayer) > 5 then EDDIELetter(CurrentPlayer)=5

        DMD "_", "EDDIE LETTER AWARDED", "", eNone, eBlink, eNone, 1500, True, ""
		QueueScene "SceneGeneralStartDef False, False, ""Eddie"", ""Eddie" & EDDIELetter(CurrentPlayer) & "L.mp4"", ""^^^^^^^^^"" ", 1000, 1
		'QueueScene "UpdateEDDIELetter", 0, 1
    End if
End Sub

Sub UpdateEDDIELetter
	dim mode
WriteToLog "     ", "UpdateEDDIELetter: " & EDDIELetter(CurrentPlayer)
    ' Update the EDDIE lights = it checks the number of letters you have and uses that number as the case.
    If EDDIELetter(CurrentPlayer)=5 then 
		if IsModeActive(kModeEddie) then 
			If IsModeQual(kModeEddie)=False Then PlaySoundVol "vo_shoottheunderworld", VolDef
			SetModeQual kModeEddie, True 
			lBattle.State = 2
			Rampup 'ready to start the mode in the Underworld
			SetupEddieInserts
		End if 
    End If
	If IsModeActive(kModeNOTB) then
		SceneGeneralStartDef True, False, "Eddie", "Eddie.mp4", "I:NOTB\\txtShootOrbits.png^^^^^^^^^"
	Else 
		if EDDIELetter(CurrentPlayer)=5 then 	' Allows Target sweep to replace this text
			SceneGeneralStartDef True, False, "Eddie", "Eddie.mp4", "I:Eddie\\txtBattleLit.png^^^^^^^^^"
		else 
			SceneGeneralStartDef True, False, "Eddie", "Eddie" & EDDIELetter(CurrentPlayer) & ".mp4", "^^^^^^^^^"
		End if 
	End if 

End Sub

Sub ResetEDDIELetter				' Enable Eddie Mode
WriteToLog "     ", "ResetEDDIELetter"
	Dim ModesStarted
	Dim i

	If IsModeActive(kModeNOTB)=False then 	' Dont start Eddie Back up if NOTB is qualified
		' See how many modes are done 
		ModesStarted=0
		for i = kModeIcarus to kModeAces
			if Mode(CurrentPlayer, i)=1 then ModesStarted=ModesStarted+1
		Next 

		EDDIELetter(CurrentPlayer) = 0
		EDDIELetterStart=True 
		if ModesStarted=1 then 
			EDDIELetter(CurrentPlayer) = 1
		Else 
			EDDIELetter(CurrentPlayer) = 0
		End if 
	End if 
'	UpdateEDDIELetter
End Sub


Sub RotateMode	' Rotate Modes (not random)
	dim i 
	Dim idx:idx=-1

WriteToLog "     ", "RotateMode: EDDIELetter=" & EDDIELetter(CurrentPlayer)
	if EDDIELetter(CurrentPlayer) = 5 then exit sub 		' Cant rotate once Eddie is spelled

	idx=GetCurrentMode()
	if idx <> -1 then Mode(CurrentPlayer, idx)=0	' Stop this one 
	SelectNextMode(idx)
End Sub 

Sub SelectNextMode(idx)
	dim i 
	if Mode(CurrentPlayer, kModeAces)=1 and Mode(CurrentPlayer, kModeFear)=1 and Mode(CurrentPlayer, kModeHallowed)=1 and _
		Mode(CurrentPlayer, kModeIcarus)=1 and Mode(CurrentPlayer, kModeRime)=1 then 
		Mode(CurrentPlayer, kMode2M2M)=2
	else 
		for i =0 to 4			' See which mode is active 
			idx=idx+1
			if idx>kModeAces then idx=kModeIcarus
			if Mode(CurrentPlayer, idx)=0 then
				Mode(CurrentPlayer, idx)=2
				exit for 
			End if 
		Next
	End if 
	UpdateModeLights
End Sub 

Sub UpdateModeLights
WriteToLog "     ", "UpdateModeLights: " & Mode(CurrentPlayer, kModeFear)

'Mode Lights 
	lMode2M2M.State 	= Mode(CurrentPlayer, kMode2M2M)  	'2MTM			' PSM not sure what this is supposed to do 
    lModeIcarus.State 	= Mode(CurrentPlayer, kModeIcarus)  'Icarus
    lModeFear.State 	= Mode(CurrentPlayer, kModeFear)  	'Fear
    lModeAces.State 	= Mode(CurrentPlayer, kModeAces)  	'Aces
    lModeHallowed.State = Mode(CurrentPlayer, kModeHallowed)'HallowedCount
    lModeRime.State 	= Mode(CurrentPlayer, kModeRime)  	'Rime
	lNOTB.State 		= Mode(CurrentPlayer, kModeNOTB)	'NOTB completion
								
End Sub


Sub StopEddieMode
	if IsModeActive(kModeEddie) then 
		SetModeActive kModeEddie, False 
		lBattle.State = 0
		PharaohBullseyeFlasherEnabled False
		RampDown
	End if 
End Sub 

Sub StartSongBattleMode		' Start a Mode
	dim bExitUp:bExitUp=False 
	Dim Mode
	Dim bMummyActive

	SetModeQual kModeEddie, False
WriteToLog "     ", "StartSongBattleMode:" & GetCurrentMode 
	Mode=GetCurrentMode

	if bMultiBallMode then		' We should never get here but if we do we have to kick out the ball
WriteToLog "     ", "ERROR... You should never get here!!!!!!!!!!!!!!!"
'MsgBox "Error You shouldnt get here! Make a note"
		PlaySoundVol "alarm", VolSfx
		vpmtimer.addtimer 1000, "Exit_Underworld False '"			'  Must kick the ball out
		exit sub 				' Cant start a mode while you are in MB
	End if 
	CheckMummyMBSave(Mode)

	StopEddieMode
	QueueFlush 0
	QueueSkip 0		' Cancel currently running clip
	DisableTombTreasure()	' TombTreaure is not available during a mode
	Select Case Mode
		case kMode2M2M:
			StartTwoMinutesToMidnight
			exit sub 					' dont eject ball until scenes play 
		case kModeIcarus: 
			StartFlightOfIcarus
		case kModeFear: 
			StartFearOfTheDark
		case kModeAces: 
			StartAcesHigh
		case kModeHallowed: 
			StartHallowed
		case kModeRime: 
			StartMariner
			bExitUp=True
	End Select  

'Andrew added if case is Mariner ramp needs to stay up
	If bExitUp Then
		TriggerScript 3600, "RampUp" 
		TriggerScript 4000, "Exit_Underworld True" ' Andrew This is working, when this mode starts it stays up
	Else	 
		TriggerScript 2000, "RampUp" 
		TriggerScript 2500, "Exit_Underworld False"
	End If
End Sub


'**************************************
' Two Minutes To Midnight - wizard mode
'**************************************
' Mode 9
' 60 seconds timed mode, X targets increase time
' complete 5 targets to light SJP Pharoah's Bullseye... rinse and repeat

Sub start2M2MScene()
	PlaySoundVol "sfx_2m2m_start", VolSfx
	SceneGeneralStartDef False, False, "Two Minutes to Midnight", "2m2m_start.mp4", "I:Two Minutes To Midnight\\txt2M2MStart.png^^^^^^^^^"
	tmr2M2MIntro.Interval=2000
	tmr2M2MIntro.UserValue=0
	tmr2M2MIntro.Enabled=True
End Sub 

Sub tmr2M2MIntro_Timer()
	dim i
	Dim Score
	tmr2M2MIntro.Interval=150
	tmr2M2MIntro.UserValue=tmr2M2MIntro.UserValue+1

	select case tmr2M2MIntro.UserValue		' Score are 1M + 15% of Mode score
		case 1: 
			PlaySoundVol "sfx_2m2m_gun", VolSfx
			Score=1000000+INT(ModePoints(CurrentPlayer, kModeFear) / 100) * 15
			PuPlayer.LabelSet pDMDFull,"lbl2M2M_1","FEAR:",1,"{'mt':2, 'color':" & RGB(116,72,117) & "}"
			PuPlayer.LabelSet pDMDFull,"txt2M2M_1", FormatScore(Score),1,"{'mt':2, 'color':" & RGB(116,72,117) & "}"
		case 2: 
			PlaySoundVol "sfx_2m2m_gun", VolSfx
			Score=1000000+INT(ModePoints(CurrentPlayer, kModeRime) / 100) * 15
			PuPlayer.LabelSet pDMDFull,"lbl2M2M_2","RIME:",1,"{'mt':2, 'color':" & RGB(67,247,166) & "}"
			PuPlayer.LabelSet pDMDFull,"txt2M2M_2", FormatScore(Score),1,"{'mt':2, 'color':" & RGB(67,247,166) & "}"
		case 3:
			PlaySoundVol "sfx_2m2m_gun", VolSfx
			Score=1000000+INT(ModePoints(CurrentPlayer, kModeHallowed) / 100) * 15
			PuPlayer.LabelSet pDMDFull,"lbl2M2M_3","HALLOWED:",1,"{'mt':2, 'color':" & RGB(215,83,19) & "}"
			PuPlayer.LabelSet pDMDFull,"txt2M2M_3", FormatScore(Score),1,"{'mt':2, 'color':" & RGB(215,83,19) & "}"
		case 4: 
			PlaySoundVol "sfx_2m2m_gun", VolSfx
			Score=1000000+INT(ModePoints(CurrentPlayer, kModeAces) / 100) * 15
			PuPlayer.LabelSet pDMDFull,"lbl2M2M_4","ACES:",1,"{'mt':2, 'color':" & RGB(73,198,255) & "}"
			PuPlayer.LabelSet pDMDFull,"txt2M2M_4", FormatScore(Score),1,"{'mt':2, 'color':" & RGB(73,198,255) & "}"
		case 5: 
			tmr2M2MIntro.Interval=2000
			PlaySoundVol "sfx_2m2m_gun", VolSfx
			Score=1000000+INT(ModePoints(CurrentPlayer, kModeIcarus) / 100) * 15
			PuPlayer.LabelSet pDMDFull,"lbl2M2M_5","ICARUS:",1,"{'mt':2, 'color':" & RGB(247,255,0) & "}"
			PuPlayer.LabelSet pDMDFull,"txt2M2M_5", FormatScore(Score),1,"{'mt':2, 'color':" & RGB(247,255,0) & "}"
		case 6:
			tmr2M2MIntro.Enabled=False
			SceneClearLabels()
			StartTwoMinutesToMidnight2()
			For i = 1 to 5 
				PuPlayer.LabelSet pDMDFull,"lbl2M2M_"&i,"",1,""
				PuPlayer.LabelSet pDMDFull,"txt2M2M_"&i, "",1,""
			Next 
			RampUp
			QueueScene2 0, "Exit_Underworld False", 500, 1, True 
	End Select 
End Sub 


Sub Scene2M2MWait()
	Dim videoStr 
	Dim JPPrompt:JPPrompt="Super Jackpot\: "
	Dim JPMult:JPMult=""
	if TwoMinutesToMidnightBonusMult >1 then 
		JPMult=" X " & TwoMinutesToMidnightBonusMult
		JPPrompt="Jackpot Value\: "
	End if 
	videoStr="2m2m_w1.mp4"
	SceneGeneralStart pDMDFull, True, False, "Two Minutes to Midnight", videoStr, "^4:Complete all shots to light SUPER JACKPOT^^^^^^^5:" & JPPrompt & FormatScore(ModePoints(CurrentPlayer, kMode2M2M)) & JPMult & "^5:X targets add mode time. ", "^" & pupColorYellow & "^^^^^^^" & pupColorRed & "^" & pupColorRed
	PuPlayer.LabelSet pDMDFull,"M2M_CNTBG","Two Minutes to Midnight\\2m2m-TimerBG.png",1,"{'mt':2, 'width':26.5, 'height':17, 'xpos':37, 'ypos':26}"

	PuPlayer.LabelSet pDMDFull,"M2M_CNT0","Two Minutes to Midnight\\1.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':39.5, 'ypos':29}"
	PuPlayer.LabelSet pDMDFull,"M2M_CNT1","Two Minutes to Midnight\\1.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':43, 'ypos':29}"
	PuPlayer.LabelSet pDMDFull,"M2M_CNT2","Two Minutes to Midnight\\5.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':48, 'ypos':29}"
	PuPlayer.LabelSet pDMDFull,"M2M_CNT3","Two Minutes to Midnight\\8.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':51, 'ypos':29}"
	PuPlayer.LabelSet pDMDFull,"M2M_CNT4","Two Minutes to Midnight\\0.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':55.5, 'ypos':29}"
	PuPlayer.LabelSet pDMDFull,"M2M_CNT5","Two Minutes to Midnight\\0.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':59, 'ypos':29}"

End Sub


Sub StartTwoMinutesToMidnight
	TurnOffDofUndercab
	DOF 202, DOFon

	PupOverlayScore

	pClearEverything

	AddEddieCard kMode2M2M, False 	' Start card animation
	QueueScene "Start2M2MScene", 5000, 1
End Sub 

Sub StartTwoMinutesToMidnight2
	'pupevent 780
	PlaySoundVol "vo_2mintomid", VolDef
    DMD "", "", "DMD_2minmode", eNone, eNone, eBlink, 1500, True, ""
    TwoMinutesToMidnightBonusCount = TwoMinutesToMidnightBonusCount + 1
	TwoMinutesToMidnightBonusMult = 0
    TwoMinutesToMidnightHitCount = 0
    TwoMinToMidnightTimerCount = 0
    EnableBallSaver 45					' Default is 45 Seconds

    'Two Minutes to Midnight Lights
    lMode2M2M.State = 0
    lModeIcarus.State = 0
    lModeFear.State = 0
    lModeAces.State = 0
    lModeRime.State = 0
    lModeHallowed.State = 0

	AddScoreMode kMode2M2M, 500000						' Default Mode Points if nothing is hit 
	ModeJPPoints(CurrentPlayer, kMode2M2M)=0
	ModePoints(CurrentPlayer, kMode2M2M)=0				' Reset Mode points

	TrooperDisable True

	SetModeActive kMode2M2M, True
	SetLightColor lMode2M2M, red, 2
	SetSlowPulse lMode2M2M
	QueueSetDefault 0, "Scene2M2MWait", "SceneClearLabels"
	SelectModeMusic kMode2M2M

	PlaySoundVol "vo_ramps2", VolDef
	DMD "_", "SHOOT RAMPS OR LOOPS", "", eNone, eBlink, eNone, 1500, True, ""
	SSetLightColor kMode2M2M, kLightRampLeft, 	yellow, 2
	SSetLightColor kMode2M2M, kLightRampRight, 	blue, 2
	SSetLightColor kMode2M2M, kLightOrbitLeft, 	orange, 2
	SSetLightColor kMode2M2M, kLightSpinnerLeft,purple, 2
	SSetLightColor kMode2M2M, kLightOrbitRight,   teal, 2

    'turn on X lights
	SSetLightColor kMode2M2M, kLightTargetX1, red, 2
	SSetLightColor kMode2M2M, kLightTargetX2, red, 2
	SSetLightColor kMode2M2M, kLightTargetX3, red, 2
	SSetLightColor kMode2M2M, kLightTargetX4, red, 2

'	Timer starts aftr e1st switch hit 
'	TwoMinToMidnightTimer.Interval = 3000				' Initial 3 second delay 
'    TwoMinToMidnightTimer.Enabled = 1
End Sub


Sub StopTwoMinutesToMidnight 'timer runs out or you lose the ball
    TwoMinToMidnightTimer.Enabled = 0
    TurnOffClock
	
	TurnOffDofUndercab
	DOF 201, DOFon 'back to gold

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kMode2M2M, False
	Mode(CurrentPlayer, kMode2M2M) = 1
	SetLightColor lMode2M2M, white, 0
	SetDefPulse lMode2M2M

    'Two Minutes to Midnight lights
    lMode2M2M.State = 0
    lModeIcarus.State = 2
    lModeFear.State = 0
    lModeAces.State = 0
    lModeRime.State = 0
    lModeHallowed.State = 0
    lNOTB.State = 0

	TrooperDisable False

'	EDDIELetter(CurrentPlayer)
	bTwoMinToMidnightCompleted(CurrentPlayer) = True 

	Mode(CurrentPlayer, kModeIcarus) = 2
	Mode(CurrentPlayer, kModeFear) = 0
	Mode(CurrentPlayer, kModeAces) = 0
	Mode(CurrentPlayer, kModeRime) = 0
	Mode(CurrentPlayer, kModeHallowed) = 0
	UpdateModeLights

    'turn on X lights
    lTargetX1.State = 2
    lTargetX2.State = 2
    lTargetX3.State = 2
    lTargetX4.State = 2

	PuPlayer.LabelSet pDMDFull,"M2M_CNTBG","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT0","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT1","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT2","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT3","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT4","PupOverlays\\clear.png",1,""
	PuPlayer.LabelSet pDMDFull,"M2M_CNT5","PupOverlays\\clear.png",1,""
	post001_IsDropped(True)

'	SceneGeneralStart pDMDFull, False, False, "Two Minutes to Midnight", "2m2m_sjp.mp4", "^^^^6:TWO MINUTES TO MIDNIGHT TOTAL^^" & FormatScore(ModePoints(CurrentPlayer, kMode2M2M)) & "^^^", "^^^^^^" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", ""2m2m_sjp.mp4"", ""^^^^6:TWO MINUTES TO MIDNIGHT TOTAL^^" & FormatScore(ModePoints(CurrentPlayer, kMode2M2M)) & "^^^"", ""^^^^^^5000:" & pupColorRed & "^^^"" ", 4000, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kMode2M2M
'	vpmtimer.addtimer 4000,"PupOverlayInGame:ModeEnd kMode2M2M '"
	ModeEnd kMode2M2M
	QueueScene2 0,"PupOverlayInGame:ModeEnd2 kMode2M2M", 10, 1, True
End Sub

Sub TwoMinToMidnightTimer_Timer 'ANDREW changed all this two match Stern (1second actually = 2 in RealTime)
	Dim TimeStr
	TwoMinToMidnightTimer.Interval = 500
	if tmrPauseTimers.Enabled then Exit sub

WriteToLog "     ", "TwoMinToMidnightTimer_Timer: " & TwoMinToMidnightTimerCount
	'pupevent 832
    'UpdateClock TwoMinToMidnightTimerCount	
	TwoMinToMidnightTimerCount = TwoMinToMidnightTimerCount + 1

	if QueueActive(0)=False then ' Only Update display when we are on the wait screen
		TimeStr=DateAdd("s",TwoMinToMidnightTimerCount, "11:58:00")
		PuPlayer.LabelSet pDMDFull,"M2M_CNTBG","Two Minutes to Midnight\\2m2m-TimerBG.png",1,"{'mt':2, 'width':26.5, 'height':17, 'xpos':37, 'ypos':26}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT0","Two Minutes to Midnight\\1.png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':39.5, 'ypos':29}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT1","Two Minutes to Midnight\\"&MID(TimeStr,2,1)&".png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':43, 'ypos':29}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT2","Two Minutes to Midnight\\"&MID(TimeStr,4,1)&".png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':48, 'ypos':29}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT3","Two Minutes to Midnight\\"&MID(TimeStr,5,1)&".png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':51, 'ypos':29}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT4","Two Minutes to Midnight\\"&MID(TimeStr,7,1)&".png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':55.5, 'ypos':29}"
		PuPlayer.LabelSet pDMDFull,"M2M_CNT5","Two Minutes to Midnight\\"&MID(TimeStr,8,1)&".png",1,"{'mt':2, 'width':3, 'height':10, 'xpos':59, 'ypos':29}"
	End if 

	If TwoMinToMidnightTimerCount = 100 Then PlaySoundVol "vo_timerunningout", VolDef
    If TwoMinToMidnightTimerCount = 120 Then 
        PlaySoundVol "vo_timeisup", VolDef
        StopTwoMinutesToMidnight

    End If
End Sub

'*******************
'  Flight of Icarus
'*******************
' Mode 4 - Ramp Combo mode
' shoot as many ramp combos in 40 seconds
' both ramps lit, shoot one to start, shoot as many combos
' each combo add a 1X and 150k to the combo value
' if the combo time runs out you start with 1x, but the value keep increasing with each shot
' Soul Shard goal: Score at least 20 million in mode points, then either survive to the end 
' or exit the mode successfully (a 4-way combo will guarantee >20m)

Sub SceneIcarusWait()
	if GetLightState(kModeIcarus, kLightRampCenter)=2 then 
		SceneGeneralStartDef True, False, "FlightOfIcarus", "Flight BG Loop.mp4", "I:FlightOfIcarus\\txt3.png^^^^^^^6:Award Value\: " & FormatScore(IcarusValue) &  "^5:Soul Shard Qualified!^"
	else 
		SceneGeneralStartDef True, False, "FlightOfIcarus", "Flight BG Loop.mp4", "I:FlightOfIcarus\\txt3.png^^^^^^^6:Award Value\: " & FormatScore(IcarusValue) &  "^5:Soul Shard in " & FormatScore(20000000 - ModePoints(CurrentPlayer, kModeIcarus)) & " Points^"
	End if 
End sub 


Sub StartFlightOfIcarus
	TurnOffDofUndercab
	DOF 203, DOfon
'	QueueScene "SceneGeneralStartDef False, False, ""FlightOfIcarus"", ""Flight Start.mp4"", ""9.5:FLIGHT OF ICARUS^^^^^^^^3:Shoow LEFT and RIGHT RAMPS^3:Combo RAMP SHOTS to multiply award!"" ", 6800, 1
	QueueScene "SceneGeneralStartDef False, False, ""FlightOfIcarus"", ""Flight Start.mp4"", ""I:FlightOfIcarus\\txt1.png^^^^^^^^^"" ", 6800, 1
	QueueScene "SceneClearLabels", 0, 1
	'pupevent 700
	PlaySoundVol "vo_flightoficarus", VolDef
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        case 0:DMD "", "", "DMD_Flightmode", eNone, eNone, eBlinkFast, 1500, True, "vo_ramps":PlaySoundVol "vo_ramps", VolDef
        case 1:DMD "", "", "DMD_Flightmode", eNone, eNone, eBlinkFast, 1500, True, "vo_ramps2":PlaySoundVol "vo_ramps2", VolDef
        case 2:DMD "", "", "DMD_Flightmode", eNone, eNone, eBlinkFast, 1500, True, "vo_ramps3":PlaySoundVol "vo_ramps3", VolDef
        case 3:DMD "", "", "DMD_Flightmode", eNone, eNone, eBlinkFast, 1500, True, "vo_ramps4":PlaySoundVol "vo_ramps4", VolDef
    End Select


	SetModeActive kModeIcarus, True
	SetLightColor lModeIcarus, noColor, 2
	SetSlowPulse lModeIcarus
	QueueSetDefault 0, "SceneIcarusWait", "SceneClearLabels"
	AddScoreMode kModeIcarus, 500000						' Default Mode Points if nothing is hit 
	SelectModeMusic kModeIcarus

	SSetLightColor kModeIcarus, kLightRampLeft, 	yellow, 2
	SSetLightColor kModeIcarus, kLightRampRight, 	yellow, 2
	SSetLightColor kModeIcarus, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeIcarus, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeIcarus, kLightSpinnerLeft,	yellow, 0
	SSetLightColor kModeIcarus, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeIcarus, kLightOrbitRight, 	yellow, 0

	IcarusValue = 2000000 '2 Mill 
    IcarusMultiplier = 1
    IcarusTimerCount = DMDFet(kDMDFet_TimerIcarus) 'seconds
	IcarusTimer.Interval=3000		' 3 second delay 
    IcarusTimer.Enabled = 1
	AddHudInfo kModeIcarus, "Flight", "Of Icarus", "", ":" & IcarusTimerCount, False

End Sub

Sub StopFlightOfIcarus 'Stops timer on drain or if time runs out - Chris	
WriteToLog "     ", "STOP ICARUS "

	TurnOffDofUndercab
	DOF 201, DOfon
    IcarusTimer.Enabled = 0 'if it was on
	IcarusCombo.Enabled = 0
	tmrShardHurryup.Enabled=False
'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeIcarus, False
	Mode(CurrentPlayer, kModeIcarus) = 1
	SetLightColor lModeIcarus, noColor, 1
	SetDefPulse lModeIcarus

	SSetLightColor kModeIcarus, kLightRampLeft, 	yellow, 0
	SSetLightColor kModeIcarus, kLightRampRight, 	yellow, 0
	SSetLightColor kModeIcarus, kLightRampCenter, 	yellow, 0

	pClearRandomTxt

	Select Case INT(RND*3)
		case 0:PlaySoundVol "vo_hahaha2", VolDef
		case 1:PlaySoundVol "vo_icarusEnd1", VolDef
		case 2:PlaySoundVol "vo_icarusEnd2", VolDef
	End Select 
'	SceneGeneralStart pDMDFull, False, False, "FlightOfIcarus", "Flight End.mp4", "^^^^I:FlightOfIcarus\\txt2.png^^" & FormatScore(ModePoints(CurrentPlayer, kModeIcarus)) & "^^^", "^^^^^^" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""FlightOfIcarus"", ""Flight End.mp4"", ""I:FlightOfIcarus\\txt2.png^^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeIcarus)) & "^^^"", ""^^^^^^3600:" & pupColorRed & "^^^"" ", 3600, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kModeIcarus
'	vpmtimer.addtimer 4000,"ModeEnd kModeIcarus '"
	ModeEnd kModeIcarus
	QueueScene2 0,"ModeEnd2 kModeIcarus", 10, 1, True

End Sub

Sub IcarusCombo_Timer 'reset the combo shots
	SSetLightColor kModeIcarus, kLightRampRight, yellow, 2
	SSetLightColor kModeIcarus, kLightRampLeft, yellow, 2
    IcarusMultiplier = 1
	IcarusCombo.Enabled = False
End Sub

Sub IcarusTimer_Timer '40 seconds
	IcarusTimer.Interval=1000
	if tmrPauseTimers.Enabled then Exit sub

	IcarusTimerCount = IcarusTimerCount - 1
	if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & IcarusTimerCount,1,""      ' Start Icarus timer count - Chris
		UpdateClock IcarusTimerCount
	End if 
	if IcarusTimerCount>0 and IcarusTimerCount <=5 then  
		PlaySoundVol "vo_" & IcarusTimerCount, VolDef
	End if 

    If IcarusTimerCount = 0 Then
		if ModePoints(CurrentPlayer, kModeIcarus) > 20000000 then 		' Light center shot for soul shard 
			IcarusTimer.Enabled=False
			StartShardHurryup(kModeIcarus)
			SSetLightColor kModeIcarus, kLightRampCenter, white, 2	
		else 
			StopFlightOfIcarus
		End if 
    End If
End Sub

'*************
'  Hallowed
'*************
' Mode 7 
' shoot one of the orange arrows first one then "Escape shot lit" he brakes cuff award 5,000,000.
'then hit shot up middle and escape awarded 5,000,000 2nd cuff broken "Escape Awarded" 5,000,000
'then hit another centre shot like sweeping the 3 drop targets gets you 20,000,000 and bars opened
'then Escape shot lit outside prison and another arrow shot for 10,000,000 and he jumps when you hit it
'then ouside prison window open light inside and shoot the pharaoh award video 
'hit that and escape awarded 60,000,000 and escape bonus 15,000,000 he crawls over wall
'then soul shard available loop and hit pharaoh again.
'if you get the soul shard right orbit lane lights for a tombtreasure award.
'so in total 6 shots.

Sub SceneHallowedWait()
	Dim AwardValue:AwardValue=5000000
	if HallowedCount=0 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW1.mp4", "7:Shoot ORANGE ARROWS!^^^^^^^^7:Award Value\: " & FormatScore(AwardValue) &  "^"
	elseif HallowedCount=1 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW2.mp4", "7:Shoot the SARCOPHAGUS!^^^^^^^^7:Award Value\: " & FormatScore(AwardValue) &  "^"
	elseif HallowedCount=2 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW3.mp4", "7:Shoot ORANGE ARROWS!^^^^^^^^7:Award Value\: " & FormatScore(AwardValue*2) &  "^"
	elseif HallowedCount=3 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW4.mp4", "I:HallowedBeThyName\\mDropTargets.png^^^^^^^^7:Award Value\: " & FormatScore(AwardValue*2) &  "^"
	elseif HallowedCount=4 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW5.mp4", "I:HallowedBeThyName\\mArrows.png^^^^^^^^7:Award Value\: " & FormatScore(AwardValue*2) &  "^"
	elseif HallowedCount=5 then
		SceneGeneralStartDef True, False, "HallowedBeThyName", "HallowedW6.mp4", "I:HallowedBeThyName\\mShootPharoh.png^^^^^^^^7:Award Value\: " & FormatScore(AwardValue*2) &  "^"
	End if 
End sub 

Sub StartHallowed
	TurnOffDofUndercab
	DOF 206, DOfon
	QueueScene "SceneGeneralStartDef False, False, ""HallowedBeThyName"", ""HallowedStart.mp4"", ""^^^^^^^^^"" ", 4000, 1
	QueueScene "SceneClearLabels", 0, 1
	'pupevent 720
	PlaySoundVol "vo_hallowed", VolDef
    DMD "", "", "DMD_Halmode", eNone, eNone, eBlinkFast, 1500, True, ""

	SetModeActive kModeHallowed, True
	SetLightColor lModeHallowed, noColor, 2
	SetSlowPulse lModeHallowed
	QueueSetDefault 0, "SceneHallowedWait", "SceneClearLabels"
	AddScoreMode kModeHallowed, 500000						' Default Mode Points if nothing is hit 
	SelectModeMusic kModeHallowed

	HallowedShots(0)=2
	HallowedShots(1)=2
	HallowedShots(2)=2
	HallowedShots(3)=2
	SSetLightColor kModeHallowed, kLightRampLeft, 	orange, HallowedShots(0)
	SSetLightColor kModeHallowed, kLightRampRight, 	orange, HallowedShots(2)
	SSetLightColor kModeHallowed, kLightLoopLeft, 	orange, 0
	SSetLightColor kModeHallowed, kLightLoopRight, 	orange, 0
	SSetLightColor kModeHallowed, kLightSpinnerLeft,orange, 0
	SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, HallowedShots(1)
	SSetLightColor kModeHallowed, kLightOrbitRight, orange, HallowedShots(3)

    PlaySoundAt "fx_resetdrop", Target005p
	DTRaise 1
	DTRaise 2
	DTRaise 3
'    Target004.IsDropped = 0 
'    Target005.IsDropped = 0 
'    Target006.IsDropped = 0 
    HallowedCount = 0
	HallowedTimer.Interval=5000				' 3 second initial delay
    HallowedTimerCount = DMDFet(kDMDFet_TimerHallowed)  'seconds
    HallowedTimer.Enabled = 1
	AddHudInfo kModeHallowed, "Hallowed Be", "Thy Name", "", ":" & HallowedTimerCount, False

End Sub

Sub StopHallowed
WriteToLog "     ", "STOP HALLOWED "
	TurnOffDofUndercab
	DOF 201, DOfon

    HallowedTimer.Enabled = 0 'if it was on
	tmrShardHurryup.Enabled=False
'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeHallowed, False
	Mode(CurrentPlayer, kModeHallowed) = 1
	SetLightColor lModeHallowed, noColor, 1
	SetDefPulse lModeHallowed


	SSetLightColor kModeHallowed, kLightRampLeft, 	orange, 0
	SSetLightColor kModeHallowed, kLightRampRight, 	orange, 0
	SSetLightColor kModeHallowed, kLightLoopLeft, 	orange, 0
	SSetLightColor kModeHallowed, kLightLoopRight, 	orange, 0
	SSetLightColor kModeHallowed, kLightSpinnerLeft,orange, 0
	SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, 0
	SSetLightColor kModeHallowed, kLightOrbitRight, orange, 0
	SSetLightColor kModeHallowed, kLightCaptiveBall, orange, 0

    PharaohBullseyeFlasherEnabled False
    TurnOffClock
	UpdateMummy2

'	SceneGeneralStart pDMDFull, False, False, "HallowedBeThyName", "HallowedFinal.mp4", "^^^^6:HALLOWED BE THY NAME TOTAL^^" & FormatScore(ModePoints(CurrentPlayer, kModeHallowed)) & "^^^", "^^^^^^" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedFinal.mp4"", ""^^^^6:HALLOWED BE THY NAME TOTAL^^" & FormatScore(ModePoints(CurrentPlayer, kModeHallowed)) & "^^^"", ""^^^^^^4000:" & pupColorRed & "^^^"" ", 4000, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kModeHallowed
'	vpmtimer.addtimer 4000,"ModeEnd kModeHallowed '"
	ModeEnd kModeHallowed
	QueueScene2 0,"ModeEnd2 kModeHallowed", 10, 1, True
End Sub

Sub HallowedTimer_Timer
	HallowedTimer.Interval=1000
	if tmrPauseTimers.Enabled then Exit sub

    HallowedTimerCount = HallowedTimerCount - 1

	if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running 
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & HallowedTimerCount,1,""      'Start HBTN timer count - Chris
		UpdateClock HallowedTimerCount
	End if 
	if HallowedTimerCount>0 and HallowedTimerCount <=5 then  
		PlaySoundVol "vo_" & HallowedTimerCount, VolDef
	End If 
    If HallowedTimerCount = 0 Then	
        StopHallowed
    End If
End Sub
'*******************************
' Rime Of The Ancient Mariner
'*******************************
' Mode 8, 2 ball multiball, one add-a-ball available
' Hurry up, count from 1 Million down, shoot Bullseye to lock on
' Hit alternate shots until you get to the Bullseye, each shot increases 500k
' Hit the Super Jackpot for a Super Jackpot
' Collect 2 super Jackpots to end the mode

Sub SceneRimeWait()
	if MarinerCount=0 then
		SceneGeneralStartDef True, False, "RimeOfTheAncientMariner", "RimeWait1.mp4", "I:RimeOfTheAncientMariner\\txt1.png^^^^^^^^9:" & FormatScore(MarinerJPValue) &  "^"
	elseif MarinerCount<4 then
		if GetLightState(kModeRime, kLightRampCenter) <> 0 then 
			SceneGeneralStartDef True, False, "RimeOfTheAncientMariner", "RimeWait2.mp4", "I:RimeOfTheAncientMariner\\txtShootBlue.png^^^^^^^9:Port Shot\: " & FormatScore(MarinerJPValue) & "^^Jackpot:" & FormatScore(MarinerJPValueStart*MarinerJPMult)
		Else 
			SceneGeneralStartDef True, False, "RimeOfTheAncientMariner", "RimeWait2.mp4", "I:RimeOfTheAncientMariner\\txtShootBlue.png^^^^^^^9:Port Shot\: " & FormatScore(MarinerJPValue) & "^^"
		End if 
	else
		PlaySoundVolLoop "sfx_RimeWait1", VolSfx
		if GetLightState(kModeRime, kLightRampCenter) <> 0 then 
			SceneGeneralStartDef True, False, "RimeOfTheAncientMariner", "RimeWait2.mp4", "I:RimeOfTheAncientMariner\\txtShootBlue.png^^^^^^^7:Starbord Shot\: " & FormatScore(MarinerJPValue) & "^^Jackpot:" & FormatScore(MarinerJPValueStart*MarinerJPMult)
		else 
			SceneGeneralStartDef True, False, "RimeOfTheAncientMariner", "RimeWait2.mp4", "I:RimeOfTheAncientMariner\\txtShootBlue.png^^^^^^^7:Starbord Shot\: " & FormatScore(MarinerJPValue) & "^^"
		End if 
	End if 

End Sub

' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=797
' https://youtu.be/zaCEffQkj4o?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1117
' https://youtu.be/besY8TS0Ges?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=3568
Sub StartMariner
	Dim tmp
	TurnOffDofUndercab
	DOF 207, DOfon
	QueueScene "SceneGeneralStartDef False, False, ""RimeOfTheAncientMariner"", ""Rime Start.mp4"", ""^^^^^^^^^"" ", 4000, 1
	QueueScene "SceneClearLabels", 0, 1
	'pupevent 800

	SetModeActive kModeRime, True
	SetLightColor lModeRime, noColor, 2
	SetSlowPulse lModeRime
	QueueSetDefault 0, "SceneRimeWait", "SceneClearLabels"
	AddScoreMode kModeRime, 500000						' Default Mode Points if nothing is hit 
	SelectModeMusic kModeRime

    PlaySoundVol "vo_rime" & INT(RND * 2)+1, VolDef
	DMD "", "", "DMD_Rimemode", eNone, eNone, eBlinkFast, 1500, True, "" 
	SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
	SSetLightColor kModeRime, kLightRampLeft, blue, 0
	SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
	SSetLightColor kModeRime, kLightLoopLeft, blue, 0
	SSetLightColor kModeRime, kLightRampCenter, blue, 0
	SSetLightColor kModeRime, kLightLoopRight, blue, 0
	SSetLightColor kModeRime, kLightRampRight, blue, 0
	SSetLightColor kModeRime, kLightOrbitRight, blue, 0

	TrooperDisable True

	MarinerProgress=0
	MarinerDifficulty=2
    MarinerCount = 0
    MarinerSJPCount = 0
	MarinerJPMult=10
    MarinerJPValue = 1000000
	MarinerJPValueStart=0

	MarinerTimer1Count=0
	MarinerTimer1.Interval=3500				' Add Initial delay
	MarinerTimer1.UserValue=20				' 20 second hurry up

	AddHudInfo kModeRime, "Rime Of The", "Ancient", "Mariner", ":" & MarinerTimer1.UserValue, False

    TriggerScript 4000, "MarinerTimer1.Enabled = 1 : PlaySoundVol ""vo_hurryupislit"", VolDef " ' Super Jackpot Timer is on
    PharaohBullseyeFlasherEnabled True
End Sub

Sub CancelMarinerMBHurryUp()
	if IsModeActive(kModeRime) and isRampUp() Then
		if MarinerTimer1.Enabled then
			MarinerJPValue=50000
			RemoveHudInfo kModeRime
			AddHudInfo kModeRime, "Rime Of The", "Ancient", "Mariner", "", True
			PuPlayer.LabelSet pDMDFull,"Msg9","",1,""
			MarinerCount = 1
			MarinerTimer1.Enabled = 0
			RampDown
			lUnderworld.State = 0
			PharaohBullseyeFlasherEnabled False
			RimeStartMB
			CheckMariner
		End if
	End if 
End Sub 

Sub StopMariner(bMBall) 
WriteToLog "     ", "StopMariner: " & bMBall
	TurnOffDofUndercab
	DOF 201, DOfon
	SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
	SSetLightColor kModeRime, kLightRampLeft, blue, 0
	SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
	SSetLightColor kModeRime, kLightLoopLeft, blue, 0
	SSetLightColor kModeRime, kLightRampCenter, blue, 0
	SSetLightColor kModeRime, kLightLoopRight, blue, 0
	SSetLightColor kModeRime, kLightRampRight, blue, 0
	SSetLightColor kModeRime, kLightOrbitRight, blue, 0

	if bMBall=1 and MarinerDifficulty<2 then	' Start the soul shard when we drop down to 1 ball
		SSetLightColor kModeRime, kLightRampCenter, DarkBlue, 2
		QueueFlush 0
		QueueScene "StartShardHurryup(kModeRime)", 0, 1
		Exit Sub 
	End if

	'pupevent 811
'	if MarinerTimer1.Enabled then RampDown			' Drained during initial hurryup - Close Ramp
	RampDown										' Always close ramp 
	MarinerTimer1.Enabled = 0
    MarinerTimer2.Enabled = 0
	PuPlayer.LabelSet pDMDFull,"Msg9","",1,""
    PharaohBullseyeFlasherEnabled False

	tmrShardHurryup.Enabled=False
'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeRime, False
	Mode(CurrentPlayer, kModeRime) = 1
	SetLightColor lModeRime, noColor, 1
	SetDefPulse lModeRime

	TrooperDisable False 					' Re-Enable Trooper 
	EnableMummy True						' Re-enable mummy 

	'TriggerScript 2000, "pupevent 812"
'	SceneGeneralStart pDMDFull, False, False, "RimeOfTheAncientMariner", "Rime End.mp4", "^^^^^^" & FormatScore(ModePoints(CurrentPlayer, kModeRime)) & "^^^", "^^^^^^" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""Rime End.mp4"", ""^^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeRime)) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 2300, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kModeRime
'	vpmtimer.addtimer 4000,"ModeEnd kModeRime '"
	ModeEnd kModeRime

	QueueFlush 0										' Mariner Default is a bit different
	QueueSkipNoDef 0

	QueueScene2 0,"ModeEnd2 kModeRime", 10, 1, True
End Sub


Sub CheckMariner
WriteToLog "     ", "CheckMariner:" & MarinerCount
	dim i 
	Dim MarinerJP
    Select Case MarinerCount
        Case 0			' Hurry Up
		case 1,2,3,4,5:
			SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
			SSetLightColor kModeRime, kLightRampLeft, blue, 0
			SSetLightColor kModeRime, kLightLoopLeft, blue, 0
			SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
			SSetLightColor kModeRime, kLightRampCenter, blue, 0
			for i = MarinerCount to MarinerCount+MarinerDifficulty
				Select case i
					case 1:SSetLightColor kModeRime, kLightSpinnerLeft, blue, 2
					case 2:SSetLightColor kModeRime, kLightRampLeft, blue, 2
					case 3:SSetLightColor kModeRime, kLightLoopLeft, blue, 2
					case 4:SSetLightColor kModeRime, kLightOrbitLeft, blue, 2
					case 5:SSetLightColor kModeRime, kLightRampCenter, blue, 2
				End Select 
			Next 

			if GetLightState(kModeRime, kLightRampCenter)<>0 then	' If Center ramp is lit open it
				RampUp 
				PharaohBullseyeFlasherEnabled True
			elseif isRampUp then 									' If we get blown off course turn this off 
				RampDown
				PharaohBullseyeFlasherEnabled False
			End if 

			if MarinerCount>1 then 
				MarinerTimer2.Enabled = 0
				MarinerTimer2.Enabled = 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""ShotPort.mp4"", ""^^^^^" & FormatScore(MarinerJPValue) & "^^^^"", ""^^^^^3000:" & pupColorRed & "^^^^"" ", 3500, 1
			End if 

        ' from now on the shots don't go back
		case 6 ' SJP - Start Right Ramps 
			PharaohBullseyeFlasherEnabled False 
			SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
			SSetLightColor kModeRime, kLightRampLeft, blue, 0
			SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
			SSetLightColor kModeRime, kLightLoopLeft, blue, 0
			SSetLightColor kModeRime, kLightRampCenter, blue, 0
			SSetLightColor kModeRime, kLightRampRight, blue, 2
			SSetLightColor kModeRime, kLightOrbitRight, blue, 2
			MarinerJP=MarinerJPValueStart*MarinerJPMult
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""RimeScope.mp4"", ""I:RimeOfTheAncientMariner\\txt2.png^^^^" & FormatScore(MarinerJP) & "^^^^^"", ""^^^^3000:" & pupColorRed & "^^^^^"" ", 1, 1
			QueueScene "PlaySoundVol ""vo_MarinerJP"", VolSfx", 3500, 1
			QueueScene "PlaySoundVol ""vo_CompletePortShots"", VolSfx", 1, 1
            MarinerTimer2.Enabled = 0
        Case 7 'Targets & Ramp
			SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
			SSetLightColor kModeRime, kLightRampLeft, blue, 0
			SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
			SSetLightColor kModeRime, kLightLoopLeft, blue, 0
			SSetLightColor kModeRime, kLightRampCenter, blue, 0
			SSetLightColor kModeRime, kLightRampRight, blue, 2
			SSetLightColor kModeRime, kLightOrbitRight, blue, 0

			SSetLightColor kModeRime, kLightLock, green, 2
			SSetLightColor kModeRime, kLightOrb, purple, 2
			SSetLightColor kModeRime, kLightBonusX, yellow, 2
			DTRaise 1
			DTRaise 2
			DTRaise 3
'			Target004.IsDropped = 0 
'			Target005.IsDropped = 0 
'			Target006.IsDropped = 0 
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""ShotStarbord.mp4"", ""^^^^^" & FormatScore(MarinerJPValue) & "^^^^"", ""^^^^^3000:" & pupColorRed & "^^^^"" ", 3500, 1
        Case 8 ' shoot the Bullesye for SJP
			SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
			SSetLightColor kModeRime, kLightRampLeft, blue, 0
			SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
			SSetLightColor kModeRime, kLightLoopLeft, blue, 0
			SSetLightColor kModeRime, kLightRampCenter, blue, 0
			SSetLightColor kModeRime, kLightLoopRight, blue, 2
			SSetLightColor kModeRime, kLightRampRight, blue, 0
			SSetLightColor kModeRime, kLightOrbitRight, blue, 0

			SSetLightColor kModeRime, kLightLock, green, 2
			SSetLightColor kModeRime, kLightOrb, purple, 2
			SSetLightColor kModeRime, kLightBonusX, yellow, 2
'			DTRaise 1
'			DTRaise 2
'			DTRaise 3
'			Target004.IsDropped = 0 
'			Target005.IsDropped = 0 
'			Target006.IsDropped = 0 
			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""ShotStarbord.mp4"", ""^^^^^" & FormatScore(MarinerJPValue) & "^^^^"", ""^^^^^3000:" & pupColorRed & "^^^^"" ", 3500, 1
        Case 9 ' shoot the Bullesye for SJP
			SSetLightColor kModeRime, kLightSpinnerLeft, blue, 0
			SSetLightColor kModeRime, kLightRampLeft, blue, 0
			SSetLightColor kModeRime, kLightOrbitLeft, blue, 0
			SSetLightColor kModeRime, kLightLoopLeft, blue, 0
			SSetLightColor kModeRime, kLightRampCenter, blue, 2
			SSetLightColor kModeRime, kLightLoopRight, blue, 2
			SSetLightColor kModeRime, kLightRampRight, blue, 0
			SSetLightColor kModeRime, kLightOrbitRight, blue, 0
			RampUp 
			ResetDrop

'			SSetLightColor kModeRime, kLightLock, green, 0
'			SSetLightColor kModeRime, kLightOrb, purple, 0
'			SSetLightColor kModeRime, kLightBonusX, yellow, 0
'			DTRaise 1
'			DTRaise 2
'			DTRaise 3
'			Target004.IsDropped = 0 
'			Target005.IsDropped = 0 
'			Target006.IsDropped = 0 
            PharaohBullseyeFlasherEnabled True
			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""ShotStarbord.mp4"", ""^^^^^" & FormatScore(MarinerJPValue) & "^^^^"", ""^^^^^3000:" & pupColorRed & "^^^^"" ", 3500, 1
        Case 10 ' won Mariner
			MarinerJP=MarinerJPValueStart*MarinerJPMult
            MarinerSJPCount = MarinerSJPCount+1
			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""RimeScope.mp4"", ""I:RimeOfTheAncientMariner\\txt2.png^^^^" & FormatScore(MarinerJP) & "^^^^^"", ""^^^^3000:" & pupColorRed & "^^^^^"" ", 1, 1
			QueueScene "PlaySoundVol ""vo_MarinerJP"", VolSfx", 3500, 1
			'pupevent 811
            PharaohBullseyeFlasherEnabled False

'			SSetLightColor kModeRime, kLightLock, green, 0
'			SSetLightColor kModeRime, kLightOrb, purple, 0
'			SSetLightColor kModeRime, kLightBonusX, yellow, 0
'			DTRaise 1
'			DTRaise 2
'			DTRaise 3
'			Target004.IsDropped = 0 
'			Target005.IsDropped = 0 
'			Target006.IsDropped = 0 
			UpdateDrop
		case 10	' Shard 
    End Select
End Sub


Dim MarinerTimer1Count
Sub MarinerTimer1_Timer 'countdown JP value
	MarinerTimer1.Interval=100				' Countdown as normal
	if tmrPauseTimers.Enabled then Exit sub

	' Every second deduct timer 
	MarinerTimer1Count=MarinerTimer1Count+1
	if MarinerTimer1Count=10 then 
		MarinerTimer1Count=0
		MarinerTimer1.UserValue=MarinerTimer1.UserValue-1
		UpdateClock MarinerTimer1.UserValue
	End If
	if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & MarinerTimer1.UserValue,1,""
	End if 

	MarinerJPValue = MarinerJPValue - 4750		' (1000000 - 50000 minimum)/20 seconds/10 MarinerTimer1Counts
	PuPlayer.LabelSet pDMDFull,"Msg9",FormatScore(MarinerJPValue),1,"" ' Countdown from 1 million on the screen for tomb award, if not hit will be granted 5k once you do hit tomb. 
 	DMD "SHOOT UNDERWORLD!", CL(1, FormatScore(MarinerJPValue)), "", eNone, eNone, eNone, 200, True, ""
	If MarinerTimer1.UserValue <= 0 Then
		MarinerJPValue=50000
		RemoveHudInfo kModeRime
		AddHudInfo kModeRime, "Rime Of The", "Ancient", "Mariner", "", True
		PuPlayer.LabelSet pDMDFull,"Msg9","",1,""
		MarinerCount = 1
		Me.Enabled = 0
		RampDown
		lUnderworld.State = 0
		EnableBallSaver DMDFet(kDMDFet_TimeRimeBallSave)
		AddMultiball 1
		PharaohBullseyeFlasherEnabled False
		RimeStartMB
		CheckMariner
	End If
End Sub

Sub MarinerTimer2_Timer 'reduces the count towards the Super Jackpot
    If MarinerCount>1 then

		If MarinerCount>3 then
			QueueScene "PlaySoundVol ""vo_timetoabandonship"",VolDef ", 1, 1
		else 
			QueueScene "PlaySoundVol ""vo_RimeNightmaresOfSea"",VolDef ", 1, 1
		End if 
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""RimeBlown.mp4"", ""I:RimeOfTheAncientMariner\\txtBlown.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 1
		QueueScene "SceneClearLabels ", 1, 1
		MarinerJPValue=MarinerJPValueStart			' Reset Jackpot 

        MarinerCount = MarinerCount -1
        CheckMariner
    End If
End Sub

'*******************************
' FearOfTheDark
'*******************************
' Mode 5
' spinners :)
' timed mode 40 seconds
' shoot 1 purple light to activate spinners
' shoot spinner and repeat until the 40 seconds run out or you loose the ball.
' Soul Shard goal: Complete three spinner rips

Dim SceneFODWait_Spinners
Sub SceneFODWait()
WriteToLog "     ", "SceneFODWait"
	Dim idx 
	idx = INT(RND * 3)+1
	if SceneFODWait_Spinners then 
		QueueScene "SceneGeneralStartDef False, False, ""FearOfTheDark"", ""FODChase" & idx & ".mp4"", ""I:FearOfTheDark\\txt2.png^^^^^^^^Spinner Value\: " & FormatScore(FearOfTheDarkSpinnerValue) & "/spin^"" ", 7967, 1
	Else 
		QueueScene "SceneGeneralStartDef False, False, ""FearOfTheDark"", ""FODChase" & idx & ".mp4"", ""^^^^^^^^^"" ", 7967, 1
	End if

End Sub

Sub SceneShard(bWait, Mode)
	Dim ModeStr
	Dim FontSize:FontSize=3
	if bWait then 
		select case mode
			case kModeRime:ModeStr    ="RIME OF THE ANCIENT MARINER":FontSize=2.5
			case kModeIcarus:ModeStr  ="FLIGHT OF ICARUS"
			case kModeHallowed:ModeStr="HALLOWED BE THY NAME":FontSize=2.7
			case kModeFear:ModeStr    ="FEAR OF THE DARK"
			case kModeAces:ModeStr    ="ACES HIGH"
		End Select 
		PlaySoundVol "vo_SoulShardIsMine", VolDef
		SceneGeneralStartDef True, False, "SoulShards", "SoulShardLoop.mp4", "^^" & FontSize & ":Shoot the PHARAOH to collect "&ModeStr&" Soul Shard^^^^^^^"

		if tmrShardHurryup.Interval=2000 then 	' Set initial text the first time 
			puPlayer.LabelSet pDMDFull,"Msg5", FormatScore(tmrShardHurryup.UserValue), 1, ""
		End if 

	Else 
		select case mode
			case kModeRime
				StartTombTreasure(kTombShardRime)
			case kModeIcarus
				StartTombTreasure(kTombShardIcarus)
			case kModeHallowed
				StartTombTreasure(kTombShardHallowed)
			case kModeFear
				StartTombTreasure(kTombShardFOD)
			case kModeAces
				StartTombTreasure(kTombShardAces)
		End Select 

'		Dont do this here - Mode End will clear this
'		RemoveHudInfo Mode

		PlaySoundVol "vo_SoulShardCollected", VolDef
		SoulShardTotal(CurrentPlayer, mode-2)=tmrShardHurryup.UserValue
		SoulShardCount(CurrentPlayer)=SoulShardCount(CurrentPlayer)+1
		SceneGeneralStart pDMDFull, False, False, "SoulShards", "SoulShard.mp4", "^^^^I:PupOverlays\\Soul Shard Collected.png^^6.5:" & FormatScore(tmrShardHurryup.UserValue) &" x 1^^6.5:" & FormatScore(tmrShardHurryup.UserValue) &"^", "^^^^^^^^4000:" & pupColorRed & "^"
		AddScoreMode Mode, tmrShardHurryup.UserValue
		' tmrShardHurryup.UserValue
	End if
End Sub 

Sub StartFearOfTheDark
	TurnOffDofUndercab
	DOF 204, DOfon
	QueueScene "SceneGeneralStartDef False, False, ""FearOfTheDark"", ""fear of the dark start.mp4"", ""I:FearOfTheDark\\txt1.png^^^^^^^^^"" ", 4000, 1
'	QueueScene "SceneClearLabels", 0, 1
	'pupevent 780
	PlaySoundVol "vo_fotd", VolDef
    DMD "", "", "DMD_Fearmode", eNone, eNone, eBlinkFast, 1500, True, ""		
    FearOfTheDarkCount = 0
	FearOfTheDarkSpinActive=False
	SceneFODWait_Spinners=False 

	GILightWhite False

	SetModeActive kModeFear, True
	SetLightColor lModeFear, noColor, 2
	SetSlowPulse lModeFear
	QueueSetDefault 0, "SceneFODWait", ""
	SelectModeMusic kModeFear

	AddScoreMode kModeFear, 500000						' Default Mode Points if nothing is hit 
	DMD "_", "SHOOT RAMPS OR LOOPS", "", eNone, eBlink, eNone, 1500, True, ""
	PlaySoundVol "vo_ramps2", VolDef
	SSetLightColor kModeFear, kLightRampLeft, 	purple, 1
	SSetLightColor kModeFear, kLightRampRight, 	purple, 1
	SSetLightColor kModeFear, kLightLoopLeft, 	purple, 1
	SSetLightColor kModeFear, kLightLoopRight, 	purple, 1
	SSetLightColor kModeFear, kLightSpinnerLeft,yellow, 0
	SSetLightColor kModeFear, kLightOrbitLeft, 	yellow, 0

	FearOfTheDarkTimer.Interval=3000		' 3 second delay 
	FearOfTheDarkTimerCount = DMDFet(kDMDFet_TimerFearOfTheDark) 'seconds 
    FearOfTheDarkTimer.Enabled = 1
	AddHudInfo kModeFear, "Fear Of", "The Dark", "", ":" & FearOfTheDarkTimerCount, False

End Sub

Sub FearOfTheDarkTimer_Timer '40 seconds
	FearOfTheDarkTimer.Interval=1000
	if tmrPauseTimers.Enabled then Exit sub

	FearOfTheDarkTimerCount = FearOfTheDarkTimerCount - 1			   'Added by Andrew to delay start 3 seconds

	if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & FearOfTheDarkTimerCount,1,""       ' Start FOTD timer count - Chris
		UpdateClock FearOfTheDarkTimerCount
	End if 
	if FearOfTheDarkTimerCount>0 and FearOfTheDarkTimerCount <=5 then  
		PlaySoundVol "vo_" & FearOfTheDarkTimerCount, VolDef
	End If 
	If FearOfTheDarkTimerCount = 0 Then 'Added by Andrew
		QueueFlush 0							' flush looping
		StopFearOfTheDark
	End If'Added by Andrew
End Sub

Sub StopFearOfTheDark
WriteToLog "     ", "StopFearOfTheDark"
	TurnOffDofUndercab
	DOF 201, DOfon

	TurnOffClock 'Timer was going negative randomly, adding the TurnOffClock - Chris
	
	GILightWhite True

	FearOfTheDarkTimer.Enabled = 0 '
	tmrShardHurryup.Enabled=False
'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeFear, False
	Mode(CurrentPlayer, kModeFear) = 1
	SetLightColor lModeFear, noColor, 1
	SetDefPulse lModeFear

	QueueFlush 0   ' - Flush Looping of FEAR (Other modes dont loop like this)

'	SceneGeneralStart pDMDFull, False, False, "FearOfTheDark", "FearOfDarkLoop.mp4", "^^^^I:FearOfTheDark\\txt3.png^^" & FormatScore(ModePoints(CurrentPlayer, kModeFear)) & "^^^", "^^^^^^3000:" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""FearOfTheDark"", ""FearOfDarkLoop.mp4"", ""I:FearOfTheDark\\txt3.png^^^^^^" & FormatScore(ModePoints(CurrentPlayer, kModeFear)) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 4000, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kModeFear
'	vpmtimer.addtimer 4000,"ModeEnd kModeFear '"
	ModeEnd kModeFear
	QueueScene2 0,"ModeEnd2 kModeFear", 10, 1, True
End Sub

'*****************
'  AcesHigh - Aces High
'*****************
' Mode 6
' 2 ball multiball with add-a-ball
' shoot the blue arrows, defeat 4 fighters
' ramp + PharaohBullseye - defeat 2 bombers
' lock on target and shoot the Bullseye
' Soul Shard goal: Collect the Super Jackpot


Sub SceneAcesWait()
	if IsModeActive(kModeAces) = False then exit sub 
	StopSound "sfx_acesLoop"
	StopSound "sfx_acesLock1"
	StopSound "sfx_acesLock2"
	if AcesHighCount<4 then
		PlaySoundVolLoop "sfx_acesLoop", VolSfx
		SceneGeneralStartDef True, False, "AcesHigh", "Aces High Mode Loop.mp4", "I:AcesHigh\\mShootBlue.png^^^^^^^^^5:"& (4-AcesHighCount) &" Fighters Remaining!"
	elseif AcesHighCount=4 or AcesHighCount=6 then
		PlaySoundVolLoop "sfx_acesLoop", VolSfx 
		SceneGeneralStartDef True, False, "AcesHigh", "Aces High Mode Loop.mp4", "I:AcesHigh\\mShootBomber.png^^^^^^^^^"
	elseif AcesHighCount=8 then
		PlaySoundVolLoop "sfx_acesLoop", VolSfx
		SceneGeneralStartDef True, False, "AcesHigh", "AcesLock.mp4", "I:AcesHigh\\mShootAceLock.png^^^^^^^^^"
	elseif AcesHighCount=9 then
		PlaySoundVolLoop "sfx_acesLock" & INT(RND*2)+1 , VolSfx 
		SceneGeneralStartDef True, False, "AcesHigh", "AcesFighterLockON.mp4", "^^^^^^^^^"
	else 
		PlaySoundVolLoop "sfx_acesLock" & INT(RND*2)+1, VolSfx 
		SceneGeneralStartDef True, False, "AcesHigh", "AcesBomberLock.mp4", "^^^^^^^^^"
	End if 

End Sub

Sub StartAcesHigh
	TurnOffDofUndercab
	DOF 205, DOfon

	QueueScene "PlaySoundVol ""sfx_acesStart"", VolSfx '" ,0, 1
	QueueScene "SceneGeneralStartDef False, False, ""AcesHigh"", ""AcesStart.mp4"", ""I:AcesHigh\\mAcesStart.png^^^^^^^^^"" ", 4000, 1
'	pupevent 850
'	vpmtimer.addtimer 3000, "pupevent 851 '" ' Andrew

	SetModeActive kModeAces, True
	SetLightColor lModeAces, noColor, 2
	SetSlowPulse lModeAces
	QueueSetDefault 0, "SceneAcesWait", "SceneClearLabels"
	SelectModeMusic kModeAces

	PlaySoundVol "vo_aceshigh", VolDef
    DMD "", "", "DMD_Acesmode", eNone, eNone, eBlinkFast, 1500, True, ""
'    For X = 0 to 7
'        SetLightColor aLightArrows(x), DarkBlue, 2
'    Next
	SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 2
	SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 2
	SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 2
	SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 2
	SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 2
	SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 2
	SSetLightColor kModeAces, kLightOrbitRight, DarkBlue, 2
	AddScoreMode kModeAces, 500000						' Default Mode Points if nothing is hit 

	EnableMummy False 
    AcesHighCount = 0
	AcesHighCompletions=0
	EnableBallSaver DMDFet(kDMDFet_TimeAcesBallSave)
    Addmultiball 1
    post001_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
    'post002_IsDropped(0)	' Not used on Premium
    vpmtimer.addtimer 2500, "PlaySoundAt""fx_SolenoidOff"", gion_bulbs_backwall: post001_IsDropped(1):post002_IsDropped(1) '"
	AddHudInfo kModeAces, "Aces", "High", "", "", True

End Sub

Dim AcesHighTimer_inc
Dim AcesHighTimer_lIndex
Sub AcesHighTimer_Timer 'a kind of "follow the lights" changes the lit arrow after 5 seconds
	Dim tmrValue
	if tmrPauseTimers.Enabled then Exit sub

WriteToLog "     ", "ACES Timer SWITCH SHOT: " & AcesHighTimer.UserValue

	if AcesHighCount=9 then 
		AcesHighTimer.UserValue=AcesHighTimer.UserValue+1

		tmrValue=5-AcesHighTimer.UserValue
		if tmrValue<0 then 		' Restart roving target
			tmrValue=0

			RemoveHudInfo kModeAces
			AddHudInfo kModeAces, "Aces", "High", "", "", True
			AcesHighCount=AcesHighCount-1
			AcesHighTimer.Enabled=False 
			SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 1
			SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 0
			SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 0
			AcesHighTimer.Interval=2000
			AcesHighTimer.UserValue=0
			AcesHighTimer_inc=1
			AcesHighTimer_lIndex=kLightRampLeft
			AcesHighTimer.Enabled=True 
		else 
			if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running
				PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & tmrValue, 1,""
				UpdateClock tmrValue
			End if 
		End if 
	else 
		SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 0
		SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 0
		SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 0
		SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 0
		SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 0
		SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 0
		SSetLightColor kModeAces, kLightOrbitRight, DarkBlue, 0
		AcesHighTimer_lIndex=AcesHighTimer_lIndex+AcesHighTimer_inc
		if AcesHighTimer_lIndex>5 then AcesHighTimer_lIndex=4:AcesHighTimer_inc=-1
		if AcesHighTimer_lIndex<0 then AcesHighTimer_lIndex=1:AcesHighTimer_inc=1
		Select case AcesHighTimer_lIndex
			case 0:SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 2
			case 1:SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 2
			case 2:SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 2
			case 3:SSetLightColor kModeAces, kLightRampCenter,  DarkBlue, 2
			case 4:SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 2
			case 5:SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 2
		End Select 
	End if 

End Sub

Sub StopAcesHigh(bMBall)       'called at the end of the multiball or when completed
WriteToLog "     ", "STOP ACES "
	TurnOffDofUndercab
	DOF 201, DOfon

	if bMBall=1 and AcesHighCompletions>0 then	' Start the soul shard
		SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 2
		QueueScene "StartShardHurryup(kModeAces)", 0, 1
		Exit Sub 
	End if 

    AcesHighTimer.Enabled = 0 'if it was on
	tmrShardHurryup.Enabled=False
'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeAces, False
	Mode(CurrentPlayer, kModeAces) = 1
	SetLightColor lModeAces, noColor, 1
	SetDefPulse lModeAces

	TrooperDisable False 					' Re-Enable Trooper 
	EnableMummy True						' Re-enable mummy 


'	SceneGeneralStart pDMDFull, False, False, "AcesHigh", "AcesStart.mp4", "^^^^I:AcesHigh\\txt1.png^^" & FormatScore(ModePoints(CurrentPlayer, kModeAces)) & "^^^", "^^^^^^3000:" & pupColorRed & "^^^"
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""AcesStart.mp4"", ""I:AcesHigh\\txt1.png^^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeAces)) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3000, 1
	QueueScene "SceneClearLabels", 0, 1
	RemoveHudInfo kModeAces
	StopSound "sfx_acesLoop"
	StopSound "sfx_acesLock1"
	StopSound "sfx_acesLock2"

	'vpmtimer.addtimer 4000,"ModeEnd kModeAces '"
	ModeEnd kModeAces
	QueueScene2 0,"ModeEnd2 kModeAces", 10, 1, True 
End Sub

Sub ModeEnd(mode)		' StopMode, EndMode
	bBallAddedThisMode=False
	SelectMusic(Songnr)	' Go back to default music
	WriteToLog "     ", "ModeEnd:" & mode & " " & GetActiveMode()

	' Wizard modes dont cycle the PrimaryMode
	if Mode <> kModeMummy and Mode<> kModeTrooper and Mode<>kModeCyborg and _
	   Mode<>kModeMadness and Mode<>kModeRTTH and Mode<>kMode2M2M then

		SelectNextMode(mode)
'		SceneClearLabels
		if GetActiveModeAll() = -1 then 					' Dont reset this if a mode was stacked
'			QueueSetDefault 0, "UpdateEDDIELetter", ""		
			ResetEDDIELetter
			SetModeActive kModeEddie, True
			SetupEddieInserts
		else 
'			' Start the respective wait scened for the previous mode back up
'			if IsModeQual(kModeTrooper) then 
'				QueueSetDefault 0, "SceneTrooperWait", "SceneClearLabels"
'			elseif IsModeQual(kModeMummy) then 
'				QueueSetDefault 0, "SceneMummyWait", "SceneClearLabels"
'			End if 
			ResetEDDIELetter
		End if 

		If(bPowerPops2(CurrentPlayer)=3)AND(bPowerSpinners2(CurrentPlayer)=3)AND(bPowerRamps2(CurrentPlayer)=3)AND(bPowerTargets2(CurrentPlayer)=3)AND(bPowerOrbits2(CurrentPlayer)=3)Then
			EnableCyborg
		End If 

		' They got the soul Shard, Cyborg Is Ready, then start Cyborg immediately  (Shortcut Cybord Start)
		if SoulShardTotal(CurrentPlayer, mode-2)<> 0 then
			If(bPowerPops2(CurrentPlayer)=3)AND(bPowerSpinners2(CurrentPlayer)=3)AND(bPowerRamps2(CurrentPlayer)=3)AND(bPowerTargets2(CurrentPlayer)=3)AND(bPowerOrbits2(CurrentPlayer)=3)Then
				if IsModeQual(kModeTrooper)=False and IsModeQual(kModeMummy) then 
					StartCyborg True
				End if 
			else
				CheckTombTreasure
			End if
		else 
			CheckTombTreasure
		End if 

	elseif GetActiveMode() = -1 then  ' Multiball Go back to how we had Eddie unless a Base Mode is still active
'		SceneClearLabels
'		QueueSetDefault 0, "UpdateEDDIELetter", ""
		SetModeActive kModeEddie, True
		if Mode <> kModeMummy and Mode<> kModeTrooper and Mode<>kModeCyborg and _
			Mode<>kModeMadness and Mode<>kModeRTTH then				' Wizards dont reset eddie except 2m2m
			ResetEDDIELetter
		else 
			If(bPowerPops2(CurrentPlayer)=3)AND(bPowerSpinners2(CurrentPlayer)=3)AND(bPowerRamps2(CurrentPlayer)=3)AND(bPowerTargets2(CurrentPlayer)=3)AND(bPowerOrbits2(CurrentPlayer)=3)Then
				EnableCyborg
			else
				CheckTombTreasure
			End if
		End if 
		SetModeActive kModeEddie, True
		SetupEddieInserts
	End if 

	if MummySaveReady(CurrentPlayer) and GetActiveModeAll() = -1 then 		' Set MummyMB Back up to Unload but only if no more modes are active 
		MummySaveReady(CurrentPlayer)=False 
		RotateRamp(2)
		SSetLightColor kModeMummy, kLightCaptiveBall, red, 2
		SetFastPulse lCaptiveBall
		CycleMummyInserts True
		CheckTombTreasure
	End if

End Sub 


Sub ModeEnd2(mode)		' StopMode, EndMode		- This basically just set the default queue 
	' Wizard modes dont cycle the PrimaryMode
	if Mode <> kModeMummy and Mode<> kModeTrooper and Mode<>kModeCyborg and _
	   Mode<>kModeMadness and Mode<>kModeRTTH and Mode<>kMode2M2M then

		if GetActiveModeAll() = -1 then 					' Dont reset this if a mode was stacked
			QueueSetDefault 0, "UpdateEDDIELetter", ""		
		else 
			' Start the respective wait scened for the previous mode back up
			if IsModeQual(kModeTrooper) then 
				QueueSetDefault 0, "SceneTrooperWait", "SceneClearLabels"
			elseif IsModeQual(kModeMummy) then 
				QueueSetDefault 0, "SceneMummyWait", "SceneClearLabels"
			End if 
		End if 

	elseif GetActiveMode() = -1 then  ' Multiball Go back to how we had Eddie unless a Base Mode is still active
		QueueSetDefault 0, "UpdateEDDIELetter", ""
	End if 
End Sub 

'********************
'   MUMMY Multiball
'********************
' Mode 1
' Light the Mummy letters
' variables used:
' MummyTimes(CurrentPlayer) how many times you have started multiball
' MummyCount(CurrentPlayer) used in light status update
' bMummySJP SuperJackpot True or False

Sub CycleMummyInserts(bEnable)
	if bEnable then 
		lMummyM.State = 0:lMummyU.State = 0:lMummyM2.State = 0:lMummyM3.State = 0:lMummyY.State = 1
		tmrMummyCycle.UserValue=5
		tmrMummyCycle.Interval = 100
		tmrMummyCycle.Enabled=True 
	else 
		tmrMummyCycle.Enabled=False 
		UpdateMummy2
	End if 
End Sub 

Sub tmrMummyCycle_Timer()
	lMummyM.State = 0:lMummyU.State = 0:lMummyM2.State = 0:lMummyM3.State = 0:lMummyY.State = 0
	tmrMummyCycle.UserValue=tmrMummyCycle.UserValue-1
	if tmrMummyCycle.UserValue=0 then tmrMummyCycle.UserValue=5
	Select case tmrMummyCycle.UserValue
		case 1:
			lMummyM.state = 1
		case 2:
			lMummyU.state = 1
		case 3:
			lMummyM2.state = 1
		case 4:
			lMummyM3.state = 1
		case 5:
			lMummyY.state = 1
	End Select 
End Sub 

Dim CycleMummyInsertsStates(5)
Sub CycleMummyInsertsState(bEnable)			' Enable/Disasble the strobing of the mummy inserts (Save states and restore when done)
WriteToLog "     ", "CycleMummyInsertsState:" & bEnable
	if bEnable then 
'		lMummyM.State=0:lMummyU.State=0:lMummyM2.State=0:lMummyM3.State=0:lMummyY.State=0

		CycleMummyInsertsStates(0)=lMummyM.State
		CycleMummyInsertsStates(1)=lMummyU.State
		CycleMummyInsertsStates(2)=lMummyM2.State
		CycleMummyInsertsStates(3)=lMummyM3.State
		CycleMummyInsertsStates(4)=lMummyY.State

		lMummyM.state = 2				' Start M Flashing 
		tmrMummyCycleState.UserValue=1
		tmrMummyCycleState.Interval = 1500
		tmrMummyCycleState.Enabled=True 
	elseif tmrMummyCycleState.Enabled then 
		lMummyM.State = CycleMummyInsertsStates(0):lMummyU.State = CycleMummyInsertsStates(1)
		lMummyM2.State = CycleMummyInsertsStates(2):lMummyM3.State = CycleMummyInsertsStates(3)
		lMummyY.State = CycleMummyInsertsStates(4)
		tmrMummyCycleState.Enabled=False 
	End if 
End Sub 

Sub tmrMummyCycleState_Timer()
	lMummyM.State = CycleMummyInsertsStates(0):lMummyU.State = CycleMummyInsertsStates(1)
	lMummyM2.State = CycleMummyInsertsStates(2):lMummyM3.State = CycleMummyInsertsStates(3)
	lMummyY.State = CycleMummyInsertsStates(4)
	tmrMummyCycleState.UserValue=tmrMummyCycleState.UserValue+1
	if tmrMummyCycleState.UserValue>5 then tmrMummyCycleState.UserValue=1
	Select case tmrMummyCycleState.UserValue
		case 1:
			lMummyM.state = 2
		case 2:
			lMummyU.state = 2
		case 3:
			lMummyM2.state = 2
		case 4:
			lMummyM3.state = 2
		case 5:
			lMummyY.state = 2
	End Select 
End Sub

Sub AddMummyletter
	Dim Multiplier
	if bMultiBallMode AND IsModeActive(kModeMummy)=False then exit sub 

	if MummyCount(CurrentPlayer)=2 and IsModeActive(kModeMummy)=False then PlaySoundVol "vo_SpellMummy", VolDef
    MummyBonusCount = MummyBonusCount + 1
    MummyCount(CurrentPlayer) = MummyCount(CurrentPlayer) + 1
'	if MummyCount(CurrentPlayer)>5 then MummyCount(CurrentPlayer)=5		' Make sure we dont overflow this somehow

WriteToLog "     ", "AddMummyletter:" & MummyCount(CurrentPlayer) & " " & bMummyLetterJP

	If IsModeActive(kModeMummy) then 
		Select Case MummyCount(CurrentPlayer)
			case 1, 3, 4:
				PlaySoundVol "vo_Mm" & INT(RND*3)+1, VolDef
			case 2:
				PlaySoundVol "vo_Um", VolDef
			case 5:
				PlaySoundVol "vo_Ym", VolDef
		End Select 
	End if
		
	Select Case MummyCount(CurrentPlayer) 'MX spelling for MUMMY captive ball
		case 1:	
			DOF 210, DOFpulse 	
		case 2:	
			DOF 210, DOFpulse	
			DOF 211, DOFpulse 	
		case 3:	
			DOF 210, DOFpulse	
			DOF 211, DOFpulse	
			DOF 212, DOFpulse	
		case 4:	
			DOF 210, DOFpulse	
			DOF 211, DOFpulse	
			DOF 212, DOFpulse	
			DOF 213, DOFpulse	
		case 5:	
			DOF 210, DOFpulse	
			DOF 211, DOFpulse	
			DOF 212, DOFpulse	
			DOF 213, DOFpulse	
			DOF 214, DOFpulse	
	End Select 

    UpdateMummy2						' Update the Inserts

	if bMummyLetterJP then 						' Show Animation
'		if MummyCount(CurrentPlayer)=5 then 	' Flush the queue so it Ramp opens immediatly
'			QueueFlush 0
'		End if 
		QueueFlush 0							' Flush the queue so it Ramp opens immediatly and JP is shown
		QueueScene "PlaySoundVol ""sfx_MummySpell2"", VolSfx ", 1, 1
		Multiplier=PlayfieldMultiplier(CurrentPlayer)

		' Callout
		if Multiplier=2 then
			QueueScene "PlaySoundVol ""vo_doublejackpot"", VolDef ", 1, 1
		elseif Multiplier=3 then
			QueueScene "PlaySoundVol ""vo_triplejackpot2"", VolDef ", 1, 1
		Else 
			QueueScene "PlaySoundVol ""vo_MummyJP" & INT(RND*3)+1 & """, VolDef ", 1, 1
		End if 

		' Actually Add the score 
		AddScore ModeJPPoints(CurrentPlayer, kModeMummy)

		' Animation
		if Multiplier<>1 then 
			QueueScene "SceneGeneralStart pDMDFull, True, False, ""MummyMultiball"", ""qual" & MummyCount(CurrentPlayer) & ".mp4"", ""I:MummyMultiball\\JP" & Multiplier & ".png^^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & " X " & Multiplier & "^^" & _
				FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)*Multiplier) & "^"", ""^^^^^^^^4000:" & pupColorRed & "^"" ", 4034, 1
		Else 
			QueueScene "SceneGeneralStart pDMDFull, True, False, ""MummyMultiball"", ""qual" & MummyCount(CurrentPlayer) & ".mp4"", ""I:MummyMultiball\\JP1.png^^^^^^6:" & _
				FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^^^"", ""^^^^^^4000:" & pupColorRed & "^^^"" ", 4034, 1
		End if 
		
	else 
		QueueFlush 0	' Flush Queue
		QueueSkip 0		' Cancel currently running clip
		if MummyCount(CurrentPlayer) <= 5 then 
			QueueScene "PlaySoundVol ""sfx_MummySpell2"", VolSfx ", 1, 1
			QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""qual" & MummyCount(CurrentPlayer) & ".mp4"", ""^^^^^^^^^"" ", 4034, 1
		End if 
	End if 

	' 
	if BallsInLock(CurrentPlayer) <> 0 then 
		SetLightColorTimed lCaptiveBall, red, 2, 1000
	else 
		SetLightColorTimed lCaptiveBall, red, 4, 1000
	End if

	if bMummyLetterJP then ' Mummy Jackpot Collected (Hit Sarcophagus after hitting switch count)
		bMummyLetterJP=False
		MummySwitchHitsMax(CurrentPlayer)=MummySwitchHitsMax(CurrentPlayer)+10
		MummySwitchHits(CurrentPlayer)=0

		if gRampPos <> 3 then 
			QueueScene "PlaySoundVol ""vo_shoottheleftramp"", VolDef ", 1, 1
	'		PlaySoundVol "vo_shoottheleftramp", VolDef
			DMD "", "", "DMD_tombt", eNone, eNone, eBlink, 1500, True, ""
'			SSetLightColor kModeMISC, kLightRampLeft, red, 2
			QueueScene2 0, "MummyEnableJP ", 0, 1, True ' Open Lock and Rotate Ramp Up
			MummyLockReady(CurrentPlayer)=True
		End if

		' Light SJP (once they spell mummy again)
		if MummyCount(CurrentPlayer)=5 then
			QueueScene "PlaySoundVol ""vo_ShootPharaohSJP" & INT(RND*2)+1 & """, VolDef ", 1, 1
			SSetLightColor kModeMummy, kLightRampCenter, red, 2
			lUnderworld.State = 2
			SetSlowPulse lUnderworld
			PharaohBullseyeFlasherEnabled True
			bMummySJP=True 
		End if 

	elseif MummyCount(CurrentPlayer)=5 then 		' Load ball to start mummy
		if BallsInLock(CurrentPlayer)=0 then 
			SetModeActive kModeMummy, True
			PlaySoundVol "vo_shoottheleftramp", VolDef
			DMD "", "", "DMD_tombt", eNone, eNone, eBlink, 1500, True, ""
'			SSetLightColor kModeMISC, kLightRampLeft, red, 2
			MummyLockReady(CurrentPlayer)=True
			QueueScene2 0, "RotateRamp(3) ", 0, 1, True ' Open Lock and Rotate Ramp Up
			QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""MStart.mp4"", ""^^^^^^^^^"" ", 6000, 1
		elseif IsModeActive(kModeMummy) then  
WriteToLog "     ", "AddMummyletter: UNLOAD"

			' Check if Other MBs are ready if so disable them
			CancelMarinerMBHurryUp

			RotateRamp(2)	' Go to unload position
			SSetLightColor kModeMummy, kLightCaptiveBall, red, 2
			SetFastPulse lCaptiveBall
			CycleMummyInserts True
		End if
	End if
End Sub

Sub tmrMummyMBHurryup_Timer()
	if tmrMummyMBHurryup.Enabled then 	' If we are not enabled then dont update because we are just clearing the lock
		tmrMummyMBHurryup.UserValue=tmrMummyMBHurryup.UserValue-1
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & tmrMummyMBHurryup.UserValue,1,""  
		UpdateClock tmrMummyMBHurryup.UserValue
	End if 

	if tmrMummyMBHurryup.UserValue=0 then 
		MummySwitchHitsDoubled(CurrentPlayer)=False
		RotateRamp(0)	' Drop Ball to underworld

		RemoveHudInfo kModeMummy  	' Go back to single HUD Info without Timer 
		AddHudInfo kModeMummy, "MUMMY", "MULTIBALL", "", "", True 

		tmrMummyMBHurryup.Enabled = False
	End if 
End Sub 

Sub SceneMummyWait()
	Dim videoStr 
	videoStr="MummyLoop.mp4"
	if bMummyLetterJP=False then 	' Jackpot / Switches 
		if bMummySJP then 			' Super Jckpot 
			SceneGeneralStart pDMDFull, True, False, "MummyMultiball", videoStr, "I:MummyMultiball\\SJPShootPharoah.png^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^^^^", "^^^^^" & pupColorRed & "^^^^"
		else 
			if QueueActive(currentqueue)=False then ' Dont overwrite if something is already in the queue
				if MummySwitchHitsDoubled(CurrentPlayer)=False then 
					if bMummySJPCnt(CurrentPlayer) >=1 then 
						SceneGeneralStart pDMDFull, True, False, "MummyMultiball", videoStr, "I:MummyMultiball\\JP.png^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^6:Jackpots Doubled^^4:Switches^4:" & MummySwitchHits(CurrentPlayer) & "/" & MummySwitchHitsMax(CurrentPlayer), "^^^^^" & pupColorRed & "^^^^"
					else 
						SceneGeneralStart pDMDFull, True, False, "MummyMultiball", videoStr, "I:MummyMultiball\\JP.png^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^^^4:Switches^4:" & MummySwitchHits(CurrentPlayer) & "/" & MummySwitchHitsMax(CurrentPlayer), "^^^^^" & pupColorRed & "^^^^"
					End if 
				Else 
					SceneGeneralStart pDMDFull, True, False, "MummyMultiball", videoStr, "I:MummyMultiball\\JP.png^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^4:Switch Hits Doubled^^4:Switches^4:" & MummySwitchHits(CurrentPlayer) & "/"& MummySwitchHitsMax(CurrentPlayer), "^^^^^" & pupColorRed & "^^^^"
				End if 
			End if
		End if 
	else 										' Jackpot / Shoot Sarcophagus
		SceneGeneralStart pDMDFull, True, False, "MummyMultiball", videoStr, "I:MummyMultiball\\JPShootSarc.png^^^^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy)) & "^^^^", "^^^^^" & pupColorRed & "^^^^"
	End if 
End Sub


Sub MummySarcLock()
WriteToLog "     ", "MummySarcLock"
	RotateRamp(2)						' Go to Unload Position
	RemoveHudInfo kModeMummy
	AddHudInfo kModeMummy, "SARCOPHAGUS", "LOCK", "", ":15", False 
	AddHudInfo kModeMummy, "MUMMY", "MULTIBALL", "", "", True
	tmrMummyMBHurryup.UserValue=15
	tmrMummyMBHurryup.Interval=1000
	tmrMummyMBHurryup.Enabled=True
End Sub

Sub CheckMummyMBSave(Mode)	' Saves state if Ramp is in MummyMB Eject mode and we start a MB
	if gRampPos=2 and BallsInLock(CurrentPlayer)>=1 and (Mode=kModeAces or Mode=kModeRime or Mode=kModeTrooper or Mode=kModeCyborg or Mode=kModeMadness or Mode=kModeNOTB) then 		' We are 1 shot from Mummy MB and These are a Multiball Mode
		MummySaveReady(CurrentPlayer)=True 
		SSetLightColor kModeMummy, kLightCaptiveBall, red, 0
		SetDefPulse lCaptiveBall
		CycleMummyInserts False
		RotateRamp(1)
	Else 
		MummySaveReady(CurrentPlayer)=False 
	End if 
End Sub

Sub StartMummyMB
WriteToLog "     ", "StartMummyMB"
	QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""MummyMB.mp4"", ""I:MummyMultiball\\txtMummyMB.png^^^^^^^^^"" ", 1067, 1
	QueueScene "SceneClearLabels", 0, 1
	tmrMummyMBHurryup.Enabled = False

	if IsModeActive(kModeEddie)=False then lBattle.State=0:RampDown		' Close the Eddie Ramp if it is open
	StopEddieMode														' Stop Eddie Mode progress if it is running

	If IsModeActive(kModeCyborg) then 				' Waiting for Cyborg
		SetModeActive kModeCyborg, False			' Disable Cyborg
		CyborgSaveStart=True
	End if

	MummyStartCnt(CurrentPlayer)=MummyStartCnt(CurrentPlayer)+1
	MummyTogglePos(CurrentPlayer)=True 

	EnableBallSaver 20
	PlaySoundVol "vo_mummymultiballf", VolDef
	DMD CL(0, "Mummy"), CL(1, "MULTIBALL"), "", eBlink, eBlink, eNone, 1500, True, ""

	SetModeQual kModeMummy, True
	QueueSetDefault 0, "SceneMummyWait", "SceneClearLabels"
	SelectModeMusic kModeMummy

	AddScoreMode kModeMummy, 500000						' Default Mode Points if nothing is hit 
	MummyScepterCnt(CurrentPlayer)=0
	MummyAddBallCnt(CurrentPlayer)=0						' Only get 3 per mummy MB
	bMummyLetterJP=False
	MummySwitchHitsDoubled(CurrentPlayer)=False
	MummySwitchHits(CurrentPlayer)=0
	ModeJPPoints(CurrentPlayer, kModeMummy)	= 1250000
	MummyCount(CurrentPlayer)=MummyMBProgress(CurrentPlayer)		' Start where we left off
	UpdateMummy2
	SSetLightColor kModeMummy, kLightCaptiveBall, red, 0	' Clear Mummy Light 
	SetDefPulse lCaptiveBall


	' Set up inserts based on progress 
	SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeMummy, kLightSpinnerLeft,	yellow, 0
	SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 0
	SSetLightColor kModeMummy, kLightRampCenter, 	yellow, 0

	If MummyAddBallMode(CurrentPlayer)=0 then
		SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightRampRight, yellow, 2
	ElseIf MummyAddBallMode(CurrentPlayer)=1 then 
		SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightRampRight, yellow, 2
		SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2

	ElseIf MummyAddBallMode(CurrentPlayer)=2 then 

		SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightRampRight, yellow, 2
		SSetLightColor kModeMummy, kLightRampCenter, yellow, 2
		SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 2
		SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
		SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2

	ElseIf MummyAddBallMode(CurrentPlayer)=3 then 
		if MummyAddBallCnt(CurrentPlayer)<2 then 			' Only get 3 per mummyMB
			SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
			SSetLightColor kModeMummy, kLightRampRight, yellow, 2
			SSetLightColor kModeMummy, kLightRampCenter, yellow, 2
			SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 2
			SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 2
			SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
			SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2
		End if 
	End if 

	AddHudInfo kModeMummy, "MUMMY", "MULTIBALL", "", "", True 
End Sub

Sub StopMummyMB
WriteToLog "     ", "STOP MummyMB :" & MummyTimes(CurrentPlayer) & " HurryUp:" & tmrMummyMBHurryup.Enabled & " " & BallsOnPlayfield & " " & BallsInRealLock

	if MummyTimes(CurrentPlayer)=0 then exit sub				' Not really started 

	if tmrMummyMBHurryup.Enabled then								' See if we need to eject the last ball  
		if BallsOnPlayfield-BallsInRealLock<>0 and BallsInRealLock<>0 then exit sub		' We dropped to 1 ball but we still have one in lock so mode isnt over.
		tmrMummyMBHurryup.Enabled=False
		tmrMummyMBHurryup.UserValue=0
		tmrMummyMBHurryup_Timer 			' Stop the timer and clear the lock, close the Ramp
	Else 
'		SSetLightColor kModeMISC, kLightRampLeft, red, 0		' Turn off this insert because we are not longer in mummy but check tomb treasure to turn it back on
		CheckTombTreasure
		if bTombTreasureReady(CurrentPlayer)=False then 	' Level Ramp and Close back gate since we cant lock a ball
			if tmrBallLock.Enabled=False then 				' Ball Lock is already moving.  Need to let it finish
				RotateRamp(1)						
			End if 
		End if 
	End if 
	tmrMummyMBHurryup.Enabled=False
	MummyTimes(CurrentPlayer)=0										' Reset for next time around
	MummyMBProgress(CurrentPlayer)=MummyCount(CurrentPlayer)		' Save our progress
	MummyLockReady(CurrentPlayer)=False

	if tmrBallLock.Enabled and BallsInRealLock>0 then 	' Lock is moving and waiting to clear
		bWaitMummyClear=True
	Else 
		bWaitMummyClear=False
	End if

	if tmrHeadOpen.Enabled then 								' A ball is being processed to drop in the lock, pause it so it doesnt fall through mech when things are being reset 
		tmrHeadOpen.Enabled=False
		VpmTimer.AddTimer 2000, "tmrHeadOpen.Enabled=True '" 
	End if 

	bMummyLetterJP=False
	SSetLightColor kModeMummy, kLightCaptiveBall, red, 0
	SetDefPulse lCaptiveBall
	SetDefPulse(lMummyM)
	SetDefPulse(lMummyU)
	SetDefPulse(lMummyM2)
	SetDefPulse(lMummyM3)
	SetDefPulse(lMummyY)

	CycleMummyInsertsState(False)
	MummyCount(CurrentPlayer)=1
	UpdateMummy2

	' Turn off all lights because when mode is ready to start it goes active and these will come back on
	SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 0
	SSetLightColor kModeMummy, kLightRampRight, yellow, 0
	SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeMummy, kLightSpinnerLeft,	yellow, 0
	SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 0

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeMummy, False
	SetModeQual kModeMummy, False

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""MummyMultiball"", ""MummyLoop.mp4"", ""I:MummyMultiball\\txtTotal.png^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeMummy)) & "^^^^"", ""^^^^^5000:" & pupColorRed & "^^^^^"" ", 5000, 1
	QueueScene "SceneClearLabels", 0, 1

	RemoveHudInfo kModeMummy
'	vpmtimer.addtimer 5000,"PupOverlayInGame:ModeEnd kModeMummy '"
	ModeEnd kModeMummy
	QueueScene2 0, "PupOverlayInGame:ModeEnd2 kModeMummy", 10,1,True
End Sub

Sub UpdateMummy2
	Dim Letter 
    Select Case MummyCount(CurrentPlayer)
        Case 0:lMummyM.State = 2:lMummyU.State = 0:lMummyM2.State = 0:lMummyM3.State = 0:lMummyY.State = 0
        Case 1:lMummyM.State = 1:lMummyU.State = 2:lMummyM2.State = 0:lMummyM3.State = 0:lMummyY.State = 0 
        Case 2:lMummyM.State = 1:lMummyU.State = 1:lMummyM2.State = 2:lMummyM3.State = 0:lMummyY.State = 0
        Case 3:lMummyM.State = 1:lMummyU.State = 1:lMummyM2.State = 1:lMummyM3.State = 2:lMummyY.State = 0 
        Case 4:lMummyM.State = 1:lMummyU.State = 1:lMummyM2.State = 1:lMummyM3.State = 1:lMummyY.State = 2 
		Case 5:lMummyM.State = 1:lMummyU.State = 1:lMummyM2.State = 1:lMummyM3.State = 1:lMummyY.State = 1
	End Select 

End Sub 

Sub MummyEnableJP()		' RotateRamp to load position (3) but make sure mummy is still enabled 
	if IsModeQual(kModeMummy) then 
		RotateRamp(3)
	End if 
End Sub 


'Sub ActivateMummySJP 'setup for superjackpot at the Pharoah's Bullseye
'    bMummySJP = True
'    SetLightColor lRampCenter, red, 0
'    PharaohBullseyeFlasherEnabled True
'	pupevent 751
'End Sub


'*********************
'  Trooper Multiball
'*********************

' Pharoah's Tomb Nest Ramp (Center Ramp)
DIM RampState 'Used to only trigger DOF Gear when changing states

Function isRampUp()
	isRampUp=(primRamp007inv_up.Collidable = True)
End Function

Sub RampUp	
WriteToLog "     ", "RampUp"
	DOF 173, DOFon 'Ramp up indicator	
	If RampState="Down" Then	
		DOF 179, DOFpulse 'Gear Motor for when ramp opens/closes	
	End If	
	RampState="Up"
    PlaySoundAt "fx_solenoidOn", Underworld
    UnderworldFlipper.RotatetoEnd

	lUnderworld.State = 2
    'lUnderworld.blinkinterval = 125:
	SetSlowPulse lUnderworld
'    Ramp007inv.Collidable = False
	primRamp007inv_up.Collidable=True 
	primRamp007inv_down.Collidable=False
End Sub

Sub RampDown
WriteToLog "     ", "RampDown"
	DOF 173, DOFoff 'Ramp up indicator off	
	If RampState="Up" Then	
		DOF 179, DOFpulse 'Gear Motor for when ramp opens/closes	
	End If	
	RampState="Down"
    PlaySoundAt "fx_solenoidOff", Underworld
    UnderworldFlipper.RotatetoStart

	lUnderworld.State = 0
    'lUnderworld.blinkinterval = 400:
	SetDefPulse lUnderworld
'    Ramp007inv.Collidable = True
	primRamp007inv_up.Collidable=False
	primRamp007inv_down.Collidable=True
'    lLock.State = 0
'    lBattle.State = 0
End Sub


'Sub SarcophagusDisable(bDisable)
'	If IsModeActive(kModeTrooper) and IsModeQual(kModeTrooper)=False and bDisable then 
'		SSetLightColor kModeTrooper, kLightOrbitLeft, green, 0
'		SSetLightColor kModeTrooper, kLightOrbitRight, green, 0
'	Elseif IsModeActive(kModeTrooper) and IsModeQual(kModeTrooper)=False and bDisable=False then 
'		SSetLightColor kModeTrooper, kLightOrbitLeft, green, 2
'		SSetLightColor kModeTrooper, kLightOrbitRight, green, 2
'	End if 
'End Sub 

Sub TrooperDisable(bDisable)
	WriteToLog "     ", "TrooperDisable:" & bDisable
	If IsModeActive(kModeTrooper) and IsModeQual(kModeTrooper)=False and bDisable then	' LockProgressing but not started
		SSetLightColor kModeTrooper, kLightOrbitLeft, green, 0
		SSetLightColor kModeTrooper, kLightOrbitRight, green, 0
		lLock.state = 1		' I saw this on a clip starting Mariner before the actual MB started and went out once MB started.  What does it mean though?
							' https://youtu.be/p2_jS8GyLSM?t=793
	Elseif IsModeActive(kModeTrooper) and IsModeQual(kModeTrooper)=False and bDisable=False then
		if IsModeActive(kModeRime)=False and IsModeActive(kModeAces)=False then 	' Rime & Aces are Special since they starts shard collect when you drain down to 1 MB
			SSetLightColor kModeTrooper, kLightOrbitLeft, green, 2
			SSetLightColor kModeTrooper, kLightOrbitRight, green, 2
			lLock.state = 0
		End if
	End if 
End Sub 

Sub SceneTrooperWait()
	SceneGeneralStart pDMDFull, True, False, "TrooperMultiball", "Wait1.mp4", "I:TrooperMultiball\\txtWait1.png^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeTrooper)) & "^^6:" & FormatScore(ModeSJPPoints(CurrentPlayer, kModeTrooper))&"^^^^^", "^^" & pupColorRed & "^^" & pupColorRed & "^^^^^"
End Sub

Sub StartTrooperMB()
	Dim i
	if IsModeActive(kModeTrooper)=False and IsModeQual(kModeTrooper)=False then

		SetModeActive kModeTrooper, True
		if bMultiBallMode=False then 
			if INT(RND*2)=0 then 
				PlaySoundVol "vo_lockIsLit2", VolDef
			Else 
				PlaySoundVol "vo_lockislit", VolDef
			End if 
			VpmTimer.AddTimer 2000, "PlaySoundVol ""vo_shootOrbits"", VolDef '"
			DMD "", "", "DMD_Lock", eNone, eNone, eBlink, 1500, True, ""  'could put an event here CP

			SSetLightColor kModeTrooper, kLightOrbitLeft, green, 2
			SSetLightColor kModeTrooper, kLightOrbitRight, green, 2
		End if 

	elseif IsModeActive(kModeTrooper)=True and IsModeQual(kModeTrooper)=False and TrooperLocks(CurrentPlayer)>=3 then		' Ready to really start Trooper MB 
		DMD CL(0, "TROOPER"), CL(1, "MULTIBALL"), "", eNone, eBlink, eNone, 1500, True, ""  'could trigger our own trooper multiblal video here

		if IsModeActive(kModeEddie)=False then lBattle.State=0:RampDown					' Close the Eddie Ramp if it is open
		StopEddieMode
		EnableBallSaver 25+12		' Add 16 seconds for start animations to go by

		Light007.state = 0
		Light006.state = 0

		SelectModeMusic kModeTrooper

		EnableMummy False 
		CheckMummyMBSave(kModeTrooper)							' Disable MummyMB and Save the state 

		AddScoreMode kModeTrooper, 500000						' Default Mode Points if nothing is hit 
		bTrooperCannonReady=True 	' First time Cannot is availabe 
		bTrooperMBFirstHit=True 	' MB Available
		TrooperProgress=0
'		For i = 0 to 7				' Rules Say Progress carries over to subsequent MB
'			TrooperMBMultiplier(CurrentPlayer, i) = 1
'		Next
		QueueSetDefault 0, "SceneTrooperWait", "SceneClearLabels"
		ModePoints(CurrentPlayer, kModeTrooper)=0						' Reset mode points
		ModeJPPoints(CurrentPlayer, kModeTrooper)=500000
		TrooperMBStartedCount(CurrentPlayer)=TrooperMBStartedCount(CurrentPlayer)+1

		' Clear all ramp lights
		SSetLightColor kModeTrooper, kLightRampLeft, 	blue, 0
		SSetLightColor kModeTrooper, kLightRampRight,   blue, 0
		SSetLightColor kModeTrooper, kLightRampCenter, 	blue, 0
		SSetLightColor kModeTrooper, kLightLoopLeft, 	blue, 0
		SSetLightColor kModeTrooper, kLightLoopRight, 	blue, 0
		SSetLightColor kModeTrooper, kLightSpinnerLeft,	blue, 0
		SSetLightColor kModeTrooper, kLightOrbitLeft, 	blue, 0
		SSetLightColor kModeTrooper, kLightOrbitRight, 	blue, 0

		' Setup colors based on overall progress
		For i = 0 to 7
			if i<>kLightRampCenter then 
				Select case TrooperMBMultiplier(CurrentPlayer, i)
					Case 1:SSetLightColor kModeTrooper, i, Blue, 2
					Case 2:SSetLightColor kModeTrooper, i, Green, 2
					Case 3:SSetLightColor kModeTrooper, i, White, 2
					Case 4:SSetLightColor kModeTrooper, i, Orange, 2
					Case 5:SSetLightColor kModeTrooper, i, Red, 2
				End Select
			End if
		Next

		SSetLightColor kModeTrooper, kLightRampCenter, 	blue, 0		' Center is never lit

		AddHudInfo kModeTrooper, "TROOPER", "MULTIBALL", "", "", True 
	End if 
End Sub 


Sub TrooperMB_CheckCannon()
	if IsModeActive(kModeTrooper)=True and IsModeQual(kModeTrooper)=True then	' Light Cannon Shot 
		if bTrooperCannonReady then 
			if GetLightState(kModeTrooper, kLightRampCenter)=0 then 	' Callout
				PlaySoundVol "vo_firethecannon", VolDef
			End if 
			StartRainbowMode kModeTrooper, kLightRampCenter
			lUnderworld.State = 2
			SetSlowPulse lUnderworld
			PharaohBullseyeFlasherEnabled True
		End if 
	End if
End Sub 


Sub StopTrooperMB
	if IsModeQual(kModeTrooper)=False then Exit Sub ' Not really started
WriteToLog "     ", "STOP TrooperMB "

	StopRainbow
	lUnderworld.State = 0
	SetDefPulse lUnderworld
	PharaohBullseyeFlasherEnabled False
	TrooperLocks(CurrentPlayer)=0

	' Turn off all lights because when we Qual this the mode is active and they will come back on
	SSetLightColor kModeTrooper, kLightRampLeft, 	blue, 0
	SSetLightColor kModeTrooper, kLightRampRight,   blue, 0
	SSetLightColor kModeTrooper, kLightLoopLeft, 	blue, 0
	SSetLightColor kModeTrooper, kLightLoopRight, 	blue, 0
	SSetLightColor kModeTrooper, kLightSpinnerLeft,	blue, 0
	SSetLightColor kModeTrooper, kLightOrbitLeft, 	blue, 0
	SSetLightColor kModeTrooper, kLightOrbitRight, 	blue, 0
	SSetLightColor kModeTrooper, kLightSJP, 		red, 0
	SSetLightColor kModeTrooper, kLightTargetX4, 	red, 0

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeTrooper, False
	SetModeQual kModeTrooper, False

	EnableMummy True

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""TrooperMultiball"", ""Total.mp4"", ""I:TrooperMultiball\\txtTotal.png^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeTrooper)) & "^^^^"", ""^^^^^5000:" & pupColorRed & "^^^^^"" ", 5000, 1
	QueueScene "SceneClearLabels", 0, 1

	RemoveHudInfo kModeTrooper
'	vpmtimer.addtimer 5000,"PupOverlayInGame:ModeEnd kModeTrooper '"
	ModeEnd kModeTrooper
	QueueScene2 0, "PupOverlayInGame:ModeEnd2 kModeTrooper", 10,1,True
End Sub


' turns on all arrows in the color of the multiplier
' aLightsArrows is a collection and must be in the right order
'Sub UpdateArrowColors
'    For X = 0 to 7
'        Select case TrooperMBMultiplier(CurrentPlayer, x)
'            Case 1:SSetLightColor kModeTrooper, aLightArrows(x), Blue, 1
'            Case 2:SSetLightColor kModeTrooper, aLightArrows(x), Green, 1
'            Case 3:SSetLightColor kModeTrooper, aLightArrows(x), Yellow, 1
'            Case 4:SSetLightColor kModeTrooper, aLightArrows(x), Orange, 1
'            Case 5:SSetLightColor kModeTrooper, aLightArrows(x), Red, 1
'        End Select
'    Next
'End Sub

'**************************
' Droptargets: extra subs
'**************************
' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=337		 Top Drop, center Drop, Hit bottom, reset and lit middle (Mode running)
' https://youtu.be/zaCEffQkj4o?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=467		 Top Drop, Bottom Drop, Hit center, awarded
Sub CheckDrops(tgtIndex)
	Dim curLight
	Dim AwardTxt
	Dim VideoStr
	Dim ImageStr
	Dim DropScore

	if IsModeActive(kModeRime) and (MarinerCount=7 or MarinerCount=8) then exit sub 	' Skip drop processing here

	Select case tgtIndex
		case 2:	' Top
			set curLight=lLightLock
		case 1:	' Middle
			set curLight=lLightOrb
		case 0:	' Bottom
			set curLight=lBonusX
	End Select

	PlaySoundVol "sfx_target" & INT(RND*3)+1, VolSfx

'WriteToLog "     ", "CheckDrops:" & DropCount & " idx:" & tgtIndex & " curLight.state:" & curLight.state
	if DropCount<3 then 
		if curLight.state=0 then 
			SetLightColorTimed curLight, noColor, 4, 600 
		elseif curLight.state=1 then
			SetLightColorTimed curLight, noColor, 3, 600
		else 
			'SetFastPulse(curLight)
			FlashForMs curLight, 500, 50, 2
			VpmTimer.AddTimer 500, "SetDefPulse " & curLight.name & " '"
			'FlashForMs curLight, 500, 50, 2
		End if 
	Else
WriteToLog "     ", "CheckDrops:" & lBonusX.state & " " & lLightOrb.state & " " & lLightLock.state & " DropPos:" & DropPos(CurrentPlayer)

		TrooperMB_CheckCannon						' See if Cannon was hit
		PharaohBullseyePharoahAward True			' PharaohAward every time your knock all 3 down 

		if DropPos(CurrentPlayer) = 0 then		' BonusX 
			VideoStr=""											' No Video 
			if bDropSweep then 					' Sweep doubles the Bonus Also
				AddBonusMultiplier 2
			Else 
				AddBonusMultiplier 1
			End if 
			PlaySoundVol "vo_Bonus" & INT(RND*2)+1, VolDef
			AwardTxt="Bonus  " & BonusMultiplier(CurrentPlayer) & "X"

		elseif DropPos(CurrentPlayer)=1 then			' LightOrb
			lOrbArrow.State = 2
			Flashforms ORBLight, 5000, 50, 0

			if MysteryLevel(CurrentPlayer) <3 then
				MysteryLevel(CurrentPlayer) = MysteryLevel(CurrentPlayer) + 1
				If QueueActive(0) then 
					PlaySoundVol "vo_mystery" & MysteryLevel(CurrentPlayer), VolDef
				else 
					PlaySoundVol "vo_orbready", VolDef
				End if 
				AwardTxt="Mystery Orb Level " & MysteryLevel(CurrentPlayer) & " is Lit"
			End if 
			VideoStr="AwardInfo.mp4"

		elseif DropPos(CurrentPlayer)=2 then			'  Light Lock 
			StartTrooperMB				' Start Pre-qual or Cannon Shot
	
			VideoStr=""											' No Video 
			AwardTxt="Trooper Locks Lit"
		end if  

        If DropValue(CurrentPlayer) <100000 Then							' Do we really Max at 100000???
            DropValue(CurrentPlayer) = DropValue(CurrentPlayer) + 10000		' Increase value 
        End If

		' Handle Resetting
		if IsModeActive(kModeTrooper) and DropPos(CurrentPlayer)=1 Then		' If Trooper Is already active then we jump back to Bonus
			DropPos(CurrentPlayer)=0
		elseif MysteryLevel(CurrentPlayer)=3 and DropPos(CurrentPlayer)=0 then 	' No more Mystery Levels 
			If IsModeActive(kModeTrooper)=False then 			' Go to trooper if we can
				DropPos(CurrentPlayer)=2
			Else 												' Fall back to bonus
				DropPos(CurrentPlayer)=0
			End if 
		Else 
			DropPos(CurrentPlayer) = (DropPos(CurrentPlayer) + 1)MOD 3
		End if 
'WriteToLog "     ", "DropPos:" & DropPos(CurrentPlayer)
		UpdateDrop

		' Add PUP Graphic
		ImageStr="I:Callouts\\txtDropTargets1.png"
		DropScore = DropBankValue(CurrentPlayer)
		if bDropSweep then 
			ImageStr="I:Callouts\\txtDropTargets2.png"
			DropScore=DropScore*2
		End if 
		AddScore DropScore

		' Show animation
		if getQueueTime(0) = 0 then 		' Nothing in the queue show it 
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", """ & VideoStr & """, """ & ImageStr & "^^^^6:" & FormatScore(DropScore) & "^^^6:" & AwardTxt & "^^"", ""^^^^5000:" & pupColorRed & "^^^^^"" ", 5000, 1
			QueueScene "SceneClearLabels", 0, 1
		Else 								' Just overlay what is in the queue now 
			SceneGeneralStart pDMDFull, False, False, "Callouts", VideoStr, ImageStr & "^^^^6:" & FormatScore(DropScore) & "^^^6:" & AwardTxt & "^^", "^^^^5000:" & pupColorRed & "^^^^^"
		End if 

		If DropBankValue(CurrentPlayer) <750000 Then						' Do we really Max at 750000???
            DropBankValue(CurrentPlayer) = DropBankValue(CurrentPlayer) + 25000
        End If

		if bDropSweep then 
			bDropSweep=False
			CheckDrops ((tgtIndex+1) Mod 3)
		else 
			vpmtimer.addtimer 1000, "ResetDrop '"
		End if 
	End if
	bDropSweep=False
	
End Sub


Sub ResetDrop
'    PlaySoundAt "fx_resetdrop", Target005p
	DTRaise 1
	DTRaise 2
	DTRaise 3
'    Target004.IsDropped = 0
'    Target005.IsDropped = 0
'    Target006.IsDropped = 0
    DropCount = 0
    UpdateDrop
End Sub

Sub UpdateDrop                                                          'lights
    Select Case DropPos(CurrentPlayer)
        Case 0:lLightLock.State = 0:lLightOrb.State = 0:lBonusX.State = 2 'Bonus X
        Case 1:lLightLock.State = 0:lLightOrb.State = 2:lBonusX.State = 1 'lit ORB for mystery award
        Case 2:lLightLock.State = 2:lLightOrb.State = 1:lBonusX.State = 1 'light lock
    End Select
End Sub

'********************
' Mystery ORB awards
'********************
' ORB lights up for mystery awards
' the award increases for each time you obtain it

Const kMystery_AddABall			=0
Const kMystery_MoreTime			=1
Const kMystery_BonusMultiplier	=2
Const kMystery_1M				=3
Const kMystery_SpotLoopAward	=6
Const kMystery_AdvanceEddie		=7
Const kMystery_BoostPJ_1M		=8
Const kMystery_AdvanceMummy		=9

Const kMystery_LightTrooperLocks=10
Const kMystery_5M				=11
Const kMystery_AdvanceRevive	=12
Const kMystery_LightEddieBattleMode	=13
Const kMystery_BoostPJ_3M		=14
Const kMystery_IncreaseBonusX	=15

Const kMystery_10M				=16
Const kMystery_IncreasePFMult	=17
Const kMystery_StartPF			=18
Const kMystery_LightRevive		=19
Const kMystery_BoostPJ_6M		=20
Const kMystery_SpotSoulShard	=21


Sub AwardMystery
	Dim MysteryAward:MysteryAward=-1
    Dim tmp
	Dim MysteryIdx
    lOrbArrow.State = 0
    LightEffect 1
	DOF 151, DOFpulse

	' Level 1 			(This displays on Instant Info when Mystery is Lit, If it is not lit it says "Mystery Orb is not lit"
	' 	Add-A-Ball
	' 	More Time
	' 	Add Bonus Multiplier
	' 	1,000,000
	' 	Spot Loop Award
	' 	Advance Eddie 
	' 	Boost Power Jackpot 1,000,000
	' 	Advance Mummy 
	' Level 2
	' 	Add-A-Ball
	' 	More Time
	'	Light Trooper Locks
	' 	5,000,000
	' 	Advance Revive	
	' 	Light Eddie Battle Mode 
	' 	Boost Power Jackpot 3,000,000
	' 		Increase Bonus Multiplier		???
	' 		Start Super Slings 				???
	' Level 3
	' 	Add-A-Ball
	' 	More Time
	' 	10,000,000
	' 	Increase Playfield Multiplier
	' 	Start a Power Feature 
	' 	Light Revive 
	' 	Boost Power Jackpot 6,000,000
	' 	Spot Soul Shard 

	' MB Modes that get Add-A-Ball
	if (IsModeActive(kModeRTTH) or (IsModeActive(kModeRime) and MarinerTimer1.Enabled=False) or IsModeActive(kModeAces)) and bBallAddedThisMode=False then 
		MysteryAward=kMystery_AddABall
	elseif IsModeActive(kModeIcarus) or IsModeActive(kModeHallowed) or IsModeActive(kModeFear) or IsModeActive(kModeAces) or IsModeActive(kMode2M2M) then
		MysteryAward=kMystery_MoreTime
	Else 
		PlaySoundVol "vo_mystery" & MysteryLevel(CurrentPlayer), VolDef
		MysteryIdx=INT(RND*6)
		If MysteryLevel(CurrentPlayer)=1 then				' Level 1 
			MysteryIdx=INT(RND*7)
			if MysteryIdx=5 and IsModeActive(kModeEddie)=False then MysteryIdx=1
			Select case MysteryIdx
				case 0:
					MysteryAward=kMystery_BonusMultiplier
				case 1:
					MysteryAward=kMystery_1M
				case 2:
					MysteryAward=kMystery_BoostPJ_1M
				case 3:
					MysteryAward=kMystery_AdvanceMummy
					MysteryAward=kMystery_1M					' TBD Make real one work
				case 4:
					MysteryAward=kMystery_SpotLoopAward
					MysteryAward=kMystery_1M					' TBD Make real one work
				case 5:
					MysteryAward=kMystery_AdvanceEddie
					MysteryAward=kMystery_1M					' TBD Make real one work
			End Select 

		elseif MysteryLevel(CurrentPlayer)=2 then 		' Level 2
			Select case MysteryIdx
				case 0:
					MysteryAward=kMystery_LightTrooperLocks
					MysteryAward=kMystery_5M					' TBD Make real one work
				case 1:
					MysteryAward=kMystery_5M
				case 2:
					if REVIVELActive(CurrentPlayer)=False or REVIVERActive(CurrentPlayer)=False then  ' Revive isnt lit
						MysteryAward=kMystery_AdvanceRevive
					else 
						MysteryAward=kMystery_5M
					End if 
				case 3:
					MysteryAward=kMystery_LightEddieBattleMode
					MysteryAward=kMystery_5M					' TBD Make real one work
				case 4:
					MysteryAward=kMystery_BoostPJ_3M
				case 5:
					MysteryAward=kMystery_IncreaseBonusX
			End Select 


		else
			Select case MysteryIdx						' Level 3
				case 0:
					MysteryAward=kMystery_10M
				case 1:
					if(PlayfieldMultiplierQual(CurrentPlayer) < MaxMultiplier) then	
						MysteryAward=kMystery_IncreasePFMult
					else 
						MysteryAward=kMystery_10M
					End if
				case 2:
					MysteryAward=kMystery_StartPF
					MysteryAward=kMystery_10M					' TBD Make real one work
				case 3:
					if REVIVELActive(CurrentPlayer)=False or REVIVERActive(CurrentPlayer)=False then  ' Revive isnt lit
						MysteryAward=kMystery_LightRevive
					else 
						MysteryAward=kMystery_10M
					End if 
				case 4:
					MysteryAward=kMystery_BoostPJ_6M
				case 5:
					MysteryAward=kMystery_SpotSoulShard
					MysteryAward=kMystery_10M					' TBD Make real one work
			End Select

		End if 
	End if 

	PlaySoundVol "sfx_mystery0", VolSfx
	Select Case MysteryAward
		case kMystery_AddABall:
			Mystery_AddaBall
		case kMystery_MoreTime:
			Mystery_AddTime
		case kMystery_BonusMultiplier:
			AddBonusMultiplier 1
			Mystery_Text("Callouts\\txtMystery1-BonusM.png")
		case kMystery_1M:
			AddScore 1000000
			Mystery_Text("Callouts\\txtMystery1-1M.png")
		case kMystery_SpotLoopAward:
			Mystery_Text("Callouts\\txtMystery1-SpotLoop.png")
		case kMystery_AdvanceEddie:
			Mystery_Text("Callouts\\txtMystery1-AdvanceEddie.png")
		case kMystery_BoostPJ_1M:	
			AddJP cPowerFeatureValue, 1000000
			Mystery_Text("Callouts\\txtMystery1-BoostPowerJP.png")
		case kMystery_AdvanceMummy:
			Mystery_Text("Callouts\\txtMystery1-AdvanceMummy.png")
		case kMystery_LightTrooperLocks:
			Mystery_Text("Callouts\\txtMystery2-LightTrooperLocks.png")
		case kMystery_5M:
			AddScore 5000000
			Mystery_Text("Callouts\\txtMystery2-5M.png")
		case kMystery_AdvanceRevive:
			REVIVECountDown(CurrentPlayer)=1
			ProcessRevive kLightSpinnerLeft			' Spot Revive
			Mystery_Text("Callouts\\txtMystery2-AdvRevive.png")
		case kMystery_LightEddieBattleMode:
			Mystery_Text("Callouts\\txtMystery2-LightEddieBattle.png")
		case kMystery_BoostPJ_3M:	
			AddJP cPowerFeatureValue, 3000000
			Mystery_Text("Callouts\\txtMystery2-BoostPowerJP.png")
		case kMystery_IncreaseBonusX:
			AddBonusMultiplier 1
			Mystery_Text("Callouts\\txtMystery2-BonusX.png")
		case kMystery_10M:
			AddScore 10000000
			Mystery_Text("Callouts\\txtMystery3-10M.png")
		case kMystery_IncreasePFMult:
			if(PlayfieldMultiplierQual(CurrentPlayer) < MaxMultiplier) then		' QUAL PFM
				PlayfieldMultiplierQual(CurrentPlayer)=PlayfieldMultiplierQual(CurrentPlayer)+1
				StartPFM
				Mystery_Text("Callouts\\txtMystery3-IncreasePFM.png")
			End if 
		case kMystery_StartPF:
			Mystery_Text("Callouts\\txtMystery3-StartPF.png")
		case kMystery_LightRevive:
			REVIVEProgress(CurrentPlayer)=5		' Light Revive
			REVIVECountDown(CurrentPlayer)=1
			ProcessRevive kLightSpinnerLeft
			Mystery_Text("Callouts\\txtMystery3-LightRevive.png")
		case kMystery_BoostPJ_6M:	
			AddJP cPowerFeatureValue, 6000000
			Mystery_Text("Callouts\\txtMystery3-BoostPowerJP")
		case kMystery_SpotSoulShard:
			Mystery_Text("Callouts\\txtMystery3-SpotSS.png")

	End Select 

'    DMD "", "", "DMD_mystery", eNone, eNone, eBlink, 1500, True, ""
'    tmp = INT(RND * 7 + MysteryLevel(CurrentPlayer)) * 150000
'    DMD CL(0, "MYSTERY AWARD"), CL(1, FormatScore(tmp)), "", eNone, eBlink, eNone, 1500, True, ""
'    Addscore tmp

	PlaySoundVol "vo_mystery" & MysteryLevel(CurrentPlayer), VolDef
	MysteryLevel(CurrentPlayer)=0
	vpmTimer.AddTimer 500, "CapWall1_Hit '"			' Finish the Cycle 
End Sub


Sub Mystery_AddTime()
	if IsModeActive(kModeIcarus) then 
		IcarusTimerCount = IcarusTimerCount+10
	elseif IsModeActive(kModeHallowed) then
		HallowedTimerCount = HallowedTimerCount +10
	elseif IsModeActive(kModeFear) then
		FearOfTheDarkTimerCount = FearOfTheDarkTimerCount + 10
	elseif IsModeActive(kModeAces) then
		AcesHighTimer.UserValue=AcesHighTimer.UserValue-5
	elseif IsModeActive(kMode2M2M) then
		TwoMinToMidnightTimerCount = TwoMinToMidnightTimerCount-5
	End if

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""AwardInfo.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ",2000, 1
	QueueScene "SceneImage ""Callouts\\txtMystery" & MysteryLevel(CurrentPlayer) & "-AddTime.png""  '", 3000, 1
	QueueScene "SceneClearLabels", 0, 1
 
End Sub 

Sub Mystery_AddaBall()
	dim bSkipAnimation:bSkipAnimation=False

	if IsModeActive(kModeRTTH) then
		' TBD Figure out what happens here 
	elseif IsModeActive(kModeRime) then
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""RimeScope.mp4"", ""I:RimeOfTheAncientMariner\\txtBallAdded.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3500, 1
		QueueScene "SceneClearLabels ", 0, 1
		bSkipAnimation=True 
	elseif IsModeActive(kModeAces) then
		QueueScene "SceneImage ""Callouts\\txtBallAdded.png""  '", 2000, 1
		QueueScene "SceneClearLabels ", 0, 1
	End if 

	if bSkipAnimation=False then 
		QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""AwardInfo.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ",2000, 1
		QueueScene "SceneImage ""Callouts\\txtMystery" & MysteryLevel(CurrentPlayer) & "-AddABall.png""  '", 3000, 1
		QueueScene "SceneClearLabels", 0, 1
	End if 

	bBallAddedThisMode=True 
	PlaySoundVol "vo_addaball", VolDef
    DMD "_", CL(1, ("ADD A BALL")), "", eNone, eBlink, eNone, 1500, True, ""

	if IsModeActive(kModeAces) or IsModeActive(kModeRime) then 
		EnableBallSaverGrace 5, False, True   ' - No grace period
	Else 
		EnableBallSaverGrace 5, True, True 
	End if 
    AddMultiball 1
    LightEffect 5

End Sub

Sub Mystery_Text(Image)
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""AwardInfo.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ",2000, 1
	QueueScene "SceneImage """ & Image & """  '", 3000, 1
	QueueScene "SceneClearLabels", 0, 1
End Sub


'*************
'  Spinners - Added Spinner Counting Chris  (JP Already had it = PowerSpinnersCount(CurrentPlayer) - ANDREW)
'*************


 
Sub Spinner001_Timer()	' Spinner Stopped 
	Spinner001.TimerEnabled = False 
	ProcessModes (kLightSpinnerStop)
	ProcessModes (kLightSpinnerStopL)
End Sub

Sub Spinner001_Spin
    If Tilted Then Exit Sub
	
	DOF 166, DOFPulse
	if Spinner001.TimerEnabled=False Then
'		SetFlash flasher001, 1, 100, 50
WriteToLog "     ", "FlashFlasher3"
		FlashFlasher1(3)
	End if 

	PlaySoundVol "sfx-spinner3", VolSfx
	PlaySoundAt "fx_spinner", Spinner001
	SpinnerBonusCount = SpinnerBonusCount + 1

	AddScore 25000

	'DMD CL(0, "SPINNERS X " & SpinnerBonusCount), "", "", eNone, eNone, eNone, 1000, True, "" ' added spinner counting - Chris
    ' run during the whole game
	CheckDeathblow 6
	ProcessModes (kLightSpinnerLeft)

	Spinner001.TimerEnabled=False 	' Start a timer to see when the spinner stops spinning 
	Spinner001.TimerInterval=1000
	Spinner001.TimerEnabled=True 

	RotateMode
	CheckPowerSpinner 2
	
End Sub

Sub Spinner002_Timer()	' Spinner Stopped 
	Spinner002.TimerEnabled = False 
	ProcessModes (kLightSpinnerStop)
End Sub

Sub Spinner002_Spin
    If Tilted Then Exit Sub

	DOF 166, DOFpulse
	if Spinner002.TimerEnabled=False then 
		SetLastSwitchHit "Spinner002"		' Do this on the first spin so we dont spam the lastSwitchHit
'		SetFlash flasher003, 3, 100, 50
		FlashFlasher1(5)
	End if 

	Spinner002.TimerEnabled=False 	' Start a timer to see when the spinner stops spinning 
	Spinner002.TimerInterval=1000
	Spinner002.TimerEnabled=True 

	AddScore 25000

	PlaySoundVol "sfx-spinner3", VolSfx
    PlaySoundAt "fx_spinner", Spinner002
	SpinnerBonusCount = SpinnerBonusCount + 1
	'DMD CL(0, "SPINNERS X " & SpinnerBonusCount), "", "", eNone, eNone, eNone, 1000, True, "" ' added spinner counting - Chris
    ' run during the whole game
	ProcessModes (kLightSpinnerRight)
    CheckPowerSpinner 1
'	UpdateDMDStats

End Sub


'********************
'   Power Features
'********************

Class clsAnimatePF
	Public x_start
	Public y_start
	Public x
	Public height
	public width
	Public inc
	Public incSizeFnt
	Public incSize
	Public fntSize
	Public phase 	' 0=Slideout, 1=Pause, 2=SlideBal
	Public Slope
	Public Label
	Public LabelTxt
	Public Image
	Public ImageG
End Class

Dim AnimatePF(5)
Set AnimatePF(0)=New clsAnimatePF
Set AnimatePF(1)=New clsAnimatePF
Set AnimatePF(2)=New clsAnimatePF
Set AnimatePF(3)=New clsAnimatePF
Set AnimatePF(4)=New clsAnimatePF
Const DoAnimatePF_EndX = 33				' x Position where the graphic stops
Const DoAnimatePF_inc = 2.5				' How fast we move 
Const DoAnimatePF_incSize = 1.1			' How fast we grow
Const DoAnimatePF_incSizeFnt = 1.09		' How fast font grows

Sub DoAnimatePF(idx)		' Animate PowerFeatures
WriteToLog "     ", "DoAnimatePF:" & idx & " " & tmrPowerStart(idx).Enabled
	Select case idx
		case 0:		' Spinners
			AnimatePF(idx).y_start=3.8
			AnimatePF(idx).x_start=1.8
			AnimatePF(idx).Label="pSpinners"
			AnimatePF(idx).LabelTxt="RemainingSpinners"
			AnimatePF(idx).Image="PuPOverlays\\PF-Spin-Lit.png"
			AnimatePF(idx).ImageG="PuPOverlays\\PF-Spin-LitG.png"
		case 1:		' Pops
			AnimatePF(idx).y_start=30
			AnimatePF(idx).x_start=1.8
			AnimatePF(idx).Label="pPops"
			AnimatePF(idx).LabelTxt="RemainingPops"
			AnimatePF(idx).Image="PuPOverlays\\PF-Pops-Lit.png"
			AnimatePF(idx).ImageG="PuPOverlays\\PF-Pops-LitG.png"
		case 2:		' Ramps
			AnimatePF(idx).y_start=16.8
			AnimatePF(idx).x_start=1.8
			AnimatePF(idx).Label="pRamps"
			AnimatePF(idx).LabelTxt="RemainingRamps"
			AnimatePF(idx).Image="PuPOverlays\\PF-Ramps-Lit.png"
			AnimatePF(idx).ImageG="PuPOverlays\\PF-Ramps-LitG.png"
		case 3:		' Targets
			AnimatePF(idx).y_start=43
			AnimatePF(idx).x_start=1.8
			AnimatePF(idx).Label="pTargets"
			AnimatePF(idx).LabelTxt="RemainingTargets"
			AnimatePF(idx).Image="PuPOverlays\\PF-Targ-Lit.png"
			AnimatePF(idx).ImageG="PuPOverlays\\PF-Targ-LitG.png"
		case 4:		' Orbits
			AnimatePF(idx).y_start=55.8
			AnimatePF(idx).x_start=1.8
			AnimatePF(idx).Label="pOrbits"
			AnimatePF(idx).LabelTxt="RemainingOrbits"
			AnimatePF(idx).Image="PuPOverlays\\PF-Orb-Lit.png"
			AnimatePF(idx).ImageG="PuPOverlays\\PF-Orb-LitG.png"
	End Select 

	AnimatePF(idx).phase=0
	AnimatePF(idx).inc=DoAnimatePF_inc			' How fast we move across the screen
	AnimatePF(idx).incSize=DoAnimatePF_incSize  ' How Fast We Grow 
	AnimatePF(idx).incSizeFnt=DoAnimatePF_incSizeFnt  ' How Fast does the font grow 
	AnimatePF(idx).fntSize=3
	AnimatePF(idx).height=15.2				' Starting Image Height 
	AnimatePF(idx).width=8.2				' Starting Image Width
	AnimatePF(idx).Slope=(AnimatePF(idx).y_start-25)/(AnimatePF(idx).x_start-DoAnimatePF_EndX)
'WriteToLog "     ", "Slope="&AnimatePF(idx).Slope
	AnimatePF(idx).x=AnimatePF(idx).x_Start
	tmrPowerStart(idx).UserValue = 0
	tmrPowerStart(idx).Interval = 50
	tmrPowerStart(idx).Enabled = True
End Sub

Sub tmrPowerStart_Timer(idx)
	dim yPos
	dim Value
	tmrPowerStart(idx).UserValue=tmrPowerStart(idx).UserValue+1

	Select case idx
		case 0:
			Value=(40*CyborgDifficulty(CurrentPlayer)+40 - PowerSpinnersCount(CurrentPlayer))
		case 1:
			Value=(20*CyborgDifficulty(CurrentPlayer)+20 - PowerPopsCount(CurrentPlayer))
		case 2:
			Value=(8*CyborgDifficulty(CurrentPlayer)+8 - PowerRampsCount(CurrentPlayer))
		case 3:
			Value=(12*CyborgDifficulty(CurrentPlayer)+12 - PowerTargetsCount(CurrentPlayer))
		case 4:
			Value=(10*CyborgDifficulty(CurrentPlayer)+10 - PowerOrbitsCount(CurrentPlayer))
	End Select 

'WriteToLog "     ", "AnimatePF(idx).x" & AnimatePF(idx).x & " Phase:" & AnimatePF(idx).phase

	if AnimatePF(idx).phase=0 and AnimatePF(idx).x>=DoAnimatePF_EndX then 
		AnimatePF(idx).inc=0
		AnimatePF(idx).incSize=1
		AnimatePF(idx).incSizeFnt=1
		tmrPowerStart(idx).UserValue=0
		AnimatePF(idx).phase=1
	elseif AnimatePF(idx).phase=1 and tmrPowerStart(idx).UserValue>40 then 		
		AnimatePF(idx).inc=-DoAnimatePF_inc
		AnimatePF(idx).incSize=1/DoAnimatePF_incSize
		AnimatePF(idx).incSizeFnt=1/DoAnimatePF_incSizeFnt
		tmrPowerStart(idx).UserValue=0
		AnimatePF(idx).phase=2
	elseif AnimatePF(idx).phase=2 and AnimatePF(idx).x<=AnimatePF(idx).x_start then 
		tmrPowerStart(idx).Enabled=False 
		puPlayer.LabelSet pTransp, AnimatePF(idx).Label&"G", "PuPOverlays\\clear.png", 1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':"&AnimatePF(idx).x_start&" ,'ypos':"& AnimatePF(idx).y_start &"}"
		puPlayer.LabelSet pDMDFull,AnimatePF(idx).Label, AnimatePF(idx).Image,1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':"&AnimatePF(idx).x_start&" ,'ypos':"& AnimatePF(idx).y_start &",'zback':1}"
		PuPlayer.LabelSet pDMDFull,AnimatePF(idx).LabelTxt ,Value, 1,"{'mt':2,'size':3,'xpos':"&AnimatePF(idx).x_start+4.2&" ,'ypos':"& AnimatePF(idx).y_start+6.2 &"}"
		Puplayer.SendMsg "{ ""mt"":301, ""SN"": " & pTransp &", ""FN"":6 }"			' Bring to Front 

		Exit Sub 
	End if

	AnimatePF(idx).fntSize=AnimatePF(idx).fntSize*AnimatePF(idx).incSizeFnt
	AnimatePF(idx).height=AnimatePF(idx).height*AnimatePF(idx).incSize
	AnimatePF(idx).width=AnimatePF(idx).width*AnimatePF(idx).incSize
	AnimatePF(idx).x=AnimatePF(idx).x+AnimatePF(idx).inc
	yPos=(AnimatePF(idx).Slope * AnimatePF(idx).x)+AnimatePF(idx).y_start

'WriteToLog "     ", "AnimatePF(idx).ImageG:" & AnimatePF(idx).Label&"G" & "=" & AnimatePF(idx).ImageG
	puPlayer.LabelSet pTransp, AnimatePF(idx).Label&"G", AnimatePF(idx).ImageG,1,"{'mt':2,'color':0,'width':"& AnimatePF(idx).width & ", 'height':" & AnimatePF(idx).height & ", 'xpos':"&AnimatePF(idx).x&" ,'ypos':"& ypos &"}"

'	(Cant call this super Fast or it kills the machine)
'	Puplayer.SendMsg "{ ""mt"":301, ""SN"": " & pTransp &", ""FN"":6 }"			' Bring to Front

	puPlayer.LabelSet pDMDFull,AnimatePF(idx).Label, AnimatePF(idx).Image,1,"{'mt':2,'color':0,'width':"& AnimatePF(idx).width & ", 'height':" & AnimatePF(idx).height & ", 'xpos':"&AnimatePF(idx).x&" ,'ypos':"& ypos &",'zback':1}"
	PuPlayer.LabelSet pDMDFull,AnimatePF(idx).LabelTxt ,Value, 1,"{'mt':2,'size':"&AnimatePF(idx).fntSize&",'xpos':"&AnimatePF(idx).x+(AnimatePF(idx).width/2)&" ,'ypos':"& ypos+(AnimatePF(idx).height/2.3) &"}"

End Sub 

Function GetPowerFeaturesComplete()
	Dim PFComplete:PFComplete=0

	if bPowerSpinners2(CurrentPlayer)=3 then PFComplete=PFComplete+1
	if bPowerPops2(CurrentPlayer)=3 then PFComplete=PFComplete+1
	if bPowerRamps2(CurrentPlayer)=3 then PFComplete=PFComplete+1
	if bPowerTargets2(CurrentPlayer)=3 then PFComplete=PFComplete+1
	if bPowerOrbits2(CurrentPlayer)=3 then PFComplete=PFComplete+1
	GetPowerFeaturesComplete=PFComplete
End Function


' shoot POPS, SPINNERS, RAMPS, TARGETS and ORBITS to power-up the Pyramid.
' remaining hits will be displayed in the grey power features display
' Once a Power Feature is complete it will change to a colour, the Power Jackpot is available to cash in at the Orb spot target 
' and that Feature’s yellow triangle insert begins pulsing, once cashed in it goes grey again with checkmark

Sub UpdatePowerFeature
	Dim PFComplete:PFComplete=0
'WriteToLog "     ", "UpdatePowerFeature: " & bPowerSpinners2(CurrentPlayer)
	if IsModeActive(kModeCyborg) then exit sub 
	if IsModeActive(kModeRTTH) then exit sub 
	if isModeActive(kModeNOTB) then exit sub


	' Take Care of Revive here 
	puPlayer.LabelSet pDMDFull,"pREVIVE","PuPOverlays\\REVIVE_" & REVIVEProgress(CurrentPlayer) & ".gif",1,"{'mt':2,'color':0,'width':12, 'height':22, 'anigif':100, 'xpos':0.1, 'ypos':72,'zback':1}"
	if REVIVELActive(CurrentPlayer)=False or REVIVERActive(CurrentPlayer)=False  then 
		puPlayer.LabelSet pDMDFull,"pREVIVESPINS",REVIVECountDown(CurrentPlayer),1,""
	else 
		puPlayer.LabelSet pDMDFull,"pREVIVESPINS"," ",1,""
	End if 


	PuPlayer.LabelSet pDMDFull,"pOverlay","PuPOverlays\\PF.png",1,"{'mt':2, 'xalign':0,'width':10.5, 'height':74, 'xpos':0.1,'ypos':1}"

   'POWER SPINNERS
	Select case bPowerSpinners2(CurrentPlayer)
		case 0:								' Default 
			puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':6,'zback':1}"
		case 1:								' Phase1Done  
			if GetLightState(kModeMISC, kLightPowerSpinners)=0 then 
				SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2		  ' Flash (normal)
				lPowerSpinnerArrow1.state = 2 ' left spinner Slow Pulse 
				lPowerSpinnerArrow2.state = 2 ' upper spinner Slow Pulse
				SetSlowPulse(lPowerSpinnerArrow1)
				SetSlowPulse(lPowerSpinnerArrow2)
				SetDefPulse(lPowerSpinner)
				'puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':6,'zback':1}"
				puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':3.8,'zback':1}"

				DoAnimatePF(0)
			elseif GetLightState(kModeMISC, kLightPowerSpinners)=2 then				' Refresh 
				puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':3.8,'zback':1}"
			End if 
		case 2:								' Phase2Done  	
			if lPowerSpinnerArrow1.state = 2 then 
				SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2			' Slow Pulse 
				lPowerSpinnerArrow1.state = 0 'off
				lPowerSpinnerArrow2.state = 0 'off
				SetSlowPulse(lPowerSpinner)

				PowerFeaturesFlash True
			End if
		case 3:								' Checked
			PFComplete=PFComplete+1
			SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 1			' Solid
			lPowerSpinnerArrow1.state = 0 'off
			lPowerSpinnerArrow2.state = 0 'off
			puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Off.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':6,'zback':1}"
			puPlayer.LabelSet pDMDFull,"pSpinnersChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
	End Select 

    'POWER POPS
	Select case bPowerPops2(CurrentPlayer)
		case 0:							' Default 
			bFlasher4Enabled=False
			puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':32,'zback':1}"
		case 1:							' Phase1Done  
			if GetLightState(kModeMISC, kLightPowerPops)=0 then
				SSetLightColor kModeMISC, kLightPowerPops, noColor, 2		' Flash (normal)
				SetDefPulse(lPowerPops)
				bFlasher4Enabled=True 										' Power pops complete 

				puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':30,'zback':1}"
				DoAnimatePF(1)
			elseif GetLightState(kModeMISC, kLightPowerPops)=2 then			' Refresh 
				bFlasher4Enabled=True
				puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':30,'zback':1}"
			End if 
		case 2:							' Phase2Done  		
			SSetLightColor kModeMISC, kLightPowerPops, noColor, 2		' Slow Pulse
			SetSlowPulse(lPowerPops)
			bFlasher4Enabled=False

			PowerFeaturesFlash True
		case 3:							' Checked 
			bFlasher4Enabled=False
			PFComplete=PFComplete+1
			SSetLightColor kModeMISC, kLightPowerPops, noColor, 1		' Solid
			puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Off.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':32,'zback':1}"
			puPlayer.LabelSet pDMDFull,"pPopsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
	End Select

    'POWER RAMPS
	Select case bPowerRamps2(CurrentPlayer)
		case 0:							' Default 
			puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':19,'zback':1}"
		case 1:							' Phase1Done  
			if GetLightState(kModeMISC, kLightPowerRamps)=0 then
				SSetLightColor kModeMISC, kLightPowerRamps, noColor, 2		  	' Flash (normal)
				lRampArrow.state = 2		' Slow Pulse 
				SetSlowPulse(lRampArrow)
				SetDefPulse(lPowerRamps)

				puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':16.8,'zback':1}"
				DoAnimatePF(2)
			elseif GetLightState(kModeMISC, kLightPowerRamps)=2 then			' Refresh 
				puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':16.8,'zback':1}"
			End if 
		case 2:							' Phase2Done  
			if lRampArrow.state = 2 then 
				SSetLightColor kModeMISC, kLightPowerRamps, noColor, 2			' Slow Pulse 
				lRampArrow.state = 0		' Off
				SetSlowPulse(lPowerRamps)

				PowerFeaturesFlash True
			End if 
		case 3:							' Checked 
			PFComplete=PFComplete+1
			SSetLightColor kModeMISC, kLightPowerRamps, noColor, 1			' Solid
			lRampArrow.state = 0		' Off 
			puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Off.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':19,'zback':1}"
			puPlayer.LabelSet pDMDFull,"pRampsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
	End Select

    'POWER TARGETS
	Select case bPowerTargets2(CurrentPlayer)
		case 0:							' Default 
			puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':45,'zback':1}"
		case 1:							' Phase1Done  
			if GetLightState(kModeMISC, kLightPowerTargets)=0 then
				SSetLightColor kModeMISC, kLightPowerTargets, noColor, 2		  	' Flash (normal)
				SetDefPulse(lPowerTargets)

				puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':43,'zback':1}"
				DoAnimatePF(3)
			elseif GetLightState(kModeMISC, kLightPowerTargets)=2 then			' Refresh 
				puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':43,'zback':1}"
			End if 
		case 2:							' Phase2Done  		
			SSetLightColor kModeMISC, kLightPowerTargets, noColor, 2			' Slow Pulse 
			SetSlowPulse(lPowerTargets)

			PowerFeaturesFlash True
		case 3:							' Checked 
			PFComplete=PFComplete+1
			SSetLightColor kModeMISC, kLightPowerTargets, noColor, 1			' Solid
			puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Off.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':45,'zback':1}"
			puPlayer.LabelSet pDMDFull,"pTargetsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
	End Select

    'POWER ORBITS
	Select case bPowerOrbits2(CurrentPlayer)
		case 0:							' Default 
			puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':58,'zback':1}"
		case 1:							' Phase1Done  
			if GetLightState(kModeMISC, kLightPowerOrbits)=0 then
				SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 2		' Flash (normal)
				lOrbitArrow.state = 2		' Slow Pulse 
				SetSlowPulse(lOrbitArrow)
				SetDefPulse(lPowerOrbits)

				puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':55.8,'zback':1}"
				DoAnimatePF(4)
			elseif GetLightState(kModeMISC, kLightPowerOrbits)=2 then			' Refresh 
				puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':55.8,'zback':1}"
			End if 
		case 2:							' Phase2Done  		
			if lOrbitArrow.state=2 then 
				SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 2		' Slow Pulse 
				lOrbitArrow.state = 0		' Off 
				SetSlowPulse(lPowerOrbits)

				PowerFeaturesFlash True
			End if 
		case 3:							' Checked
			PFComplete=PFComplete+1
			SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 1		' Solid
			lOrbitArrow.state = 0		' Off 
			puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Off.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':58,'zback':1}"
			puPlayer.LabelSet pDMDFull,"pOrbitsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
	End Select


    'Super POWER jackpot - only the active pyramids count the X
    PowerJackpotMultiplier = 1
    If bPowerPops2(CurrentPlayer)=2 Then
		PowerJackpotMultiplier = PowerJackpotMultiplier + 1
    End If
    If bPowerSpinners2(CurrentPlayer)=2 Then		
        PowerJackpotMultiplier = PowerJackpotMultiplier + 1
    End If
    If bPowerRamps2(CurrentPlayer)=2 Then		
        PowerJackpotMultiplier = PowerJackpotMultiplier + 1
    End If
    If bPowerTargets2(CurrentPlayer)=2 Then		
        PowerJackpotMultiplier = PowerJackpotMultiplier + 1
    End If
    If bPowerOrbits2(CurrentPlayer)=2 Then		
        PowerJackpotMultiplier = PowerJackpotMultiplier + 1
    End If

    'check for power jackpot enable
	If PowerJackpotMultiplier >1 AND PowerJackpotMultiplier < 6 Then 'at least one Power feature is activated
        bPJActivated = True
		bSPJActivated = False
		PlaySoundVol "vo_powerjackpotislit", VolDef
        DMD "", "", "DMD_Superjl", eNone, eNone, eBlink, 1500, True, "" 'enabled PJ
        Light045.State = 2 'flash the orb light 
		'check for super power jackpot enable
	Elseif PowerJackpotMultiplier = 6 Then 'all power features are activated for super power jackpot
		bPJActivated = False
		bSPJActivated = True
		PlaySoundVol "vo_superjackpotislit", VolDef
		DMD "", "", "DMD_Superjl", eNone, eNone, eBlink, 1500, True, "" 'enabled SPJ
		Light045.State = 2 'flash the orb light 
	Else
		bSPJActivated = False
		Light045.State = 0
	End If

WriteToLog "     ", "UpdatePowerFeature: " & PFComplete
	if PFComplete=3 and bPowerFeatureEBCollected(CurrentPlayer)=False then
		bPowerFeatureEBCollected(CurrentPlayer)=True
		LightExtraBall()
	End if 

End Sub

Sub StopPowerFeature 'called at the end of the ball or after the SPJ award
WriteToLog "     ", "StopPowerFeature"
    PowerJackpotMultiplier = 1
    PowerFeatureValue = 0
    bSPJActivated = 0
	bPJActivated = 0

    Light045.State = 0
    'pops
    If bPowerPops2(CurrentPlayer) =2 Then
        bPowerPops2(CurrentPlayer) = 3
	End If
    ' spinners
    If bPowerSpinners2(CurrentPlayer)=2 Then
        bPowerSpinners2(CurrentPlayer) = 3
    End If
    'ramps
    If bPowerRamps2(CurrentPlayer)=2 Then
        bPowerRamps2(CurrentPlayer) = 3
    End If
    'targets
    If bPowerTargets2(CurrentPlayer)=2 Then
        bPowerTargets2(CurrentPlayer) = 3
    End If
    'orbits
    If bPowerOrbits2(CurrentPlayer)=2 Then
        bPowerOrbits2(CurrentPlayer) = 3
    End If
   'If(bPowerPops(CurrentPlayer) = 1)AND(bPowerSpinners(CurrentPlayer) = 1)AND(bPowerRamps(CurrentPlayer) = 1)AND(bPowerTargets(CurrentPlayer) = 1)AND(bPowerOrbits(CurrentPlayer) = 1)Then
    '    ResetPowerFeatures ' this resets all the power counts AND the deactivates the previously completed power feature.  (This should NOT happen unless Cyborg is started!) - this is breaking cyborg starting
   ' End If

	UpdatePowerFeature
End Sub

Sub ResetPowerFeatures
    for x = 0 to 4
        PowerSpinnersCount(x) = 0
        PowerPopsCount(x)	= 0
        PowerRampsCount(x)	= 0
        PowerTargetsCount(x)= 0
        PowerOrbitsCount(x)	= 0
        bPowerPops(x) 		= 0
		bPowerSpinners2(x)	= 0
        bPowerPops2(x) 		= 0
        bPowerRamps2(x) 	= 0
        bPowerTargets2(x) 	= 0
        bPowerOrbits2(x) 	= 0
    Next
    UpdatePowerFeature
	UpdateDMDStats
End Sub

Sub AwardPJackpot ' Power Jackpot
	Dim JPtxt
WriteToLog "     ", "AwardPJackpot"
'	AddBonusMultiplier 1
	Dim tmp
	tmp = PowerFeatureValue * PowerJackpotMultiplier * PlayfieldMultiplier(CurrentPlayer)

	JPtxt=FormatScore(tmp)
	QueueScene "PlaySoundVol ""sfx_bullseye"",VolSfx ", 0, 1
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""Bullseye.mp4"", ""I:Callouts\\txtPowerJP.png^^^^^^8:" & JPtxt & "^^^"", ""^^^^^^4000:" & pupColorYellow & "^^^"" ", 4000, 1
	QueueScene "SceneClearLabels ", 0, 1

	PlaySoundVol "vo_powerjackpotcollected", VolDef
	DMD CL(0, FormatScore(tmp)), "POWER JACKPOT", "", eNone, eBlinkFast, eNone, 1500, True, ""
	DOF 126, DOFPulse
	Addscore PowerFeatureValue * PowerJackpotMultiplier
	LightEffect 2
	StopPowerFeature
	' Only when you collect the PJackpot you may start Cyborg
'WriteToLog "     ", "AwardPJackpot Pops:" & bPowerPops2(CurrentPlayer)
'WriteToLog "     ", "AwardPJackpot Spinners:" & bPowerSpinners2(CurrentPlayer)
'WriteToLog "     ", "AwardPJackpot Ramps:" & bPowerRamps2(CurrentPlayer)
'WriteToLog "     ", "AwardPJackpot Targets:" & bPowerTargets2(CurrentPlayer)
'WriteToLog "     ", "AwardPJackpot Orbits:" & bPowerOrbits2(CurrentPlayer)

	If(bPowerPops2(CurrentPlayer)=3)AND(bPowerSpinners2(CurrentPlayer)=3)AND(bPowerRamps2(CurrentPlayer)=3)AND(bPowerTargets2(CurrentPlayer)=3)AND(bPowerOrbits2(CurrentPlayer)=3)Then
		if GetActiveModeAll() = -1 then 	' No modes active
			EnableCyborg
		End if 
	End If

	vpmTimer.AddTimer 500, "CapWall1_Hit '"			' Finish the Cycle 

End Sub

'***********************************
' Cyborg Multiball - Wizard Mode
'***********************************
'starts after Completing all Power Features
'Shoot the center ramp to start. 
'During Cyborg MB, light all of the Pyramid Segments (spinners, orbits, ramps, targets, and bumpers) 
'Hitthem each at least once to light the Super Jackpot at the Orb. 
'Collecting the SJP awards an Add-a-Ball for the first two times the shot is made. 
'Every Power shot increases the value of the SJP
'The SJP multiplier increases every time the Super Jackpot is collected.
'Starting Cyborg MB lights the Cyborg Eddie card
'Scoring a 5X SJP nets you a Level 2 Cyborg Eddie card. 
'The Cyborg MB SJP multiplier level does not carry between subsequent Cyborg multiballs.
'The Super Jackpot starts at 4,000,000 (sometimes it can start above 4M
'You may not qualify any PFx during Cyborg, but you may activate a PFx you qualified prior to starting Cyborg.

' Mode 3

Sub SceneCyborgWait()
	Dim videoStr 
	Dim Multiplier:Multiplier=""
	videoStr="Start2.mp4"
	if CyborgSJPMultiplier<>1 then Multiplier=" x " & CyborgSJPMultiplier 
	SceneGeneralStart pDMDFull, True, False, "CyborgMultiball", videoStr, "I:CyborgMultiball\\txtStart.png^^^^^^^^" & FormatScore(CyborgSJPValue(Currentplayer)) & Multiplier & "^", "^^^^^^^^" & pupColorRed & "^" 
End Sub


Sub EnableCyborg
	Dim JPtxt
WriteToLog "     ", "EnableCyborg"

	AddEddieCard kModeCyborg, False				' Get an eddie card just for qualifying 

	if bMultiBallMode then 			' Dont Finish Enable until we get out of MB
		CyborgSaveStart=True 
		Exit Sub 
	End if 

    DMD "BULLSEYE STARTS", "CYBORG MULTIBALL", "", eBlink, eBlink, eNone, 1500, True, ""
	SetModeActive kModeCyborg, True 
	SetModeQual kModeCyborg, False
	RampUp 
    SetLightColor lRampCenter, red, 2
	SetSlowPulse lRampCenter
	lBattle.state = 2
	lLock.state = 2

	SetSlowPulse light014:light014.state = 2	' Cyborg ? Light
	SetSlowPulse light072:light072.state = 2	' Lightning Left 
	SetSlowPulse light073:light073.state = 2	' Lightning Right

	bPharaohBullseyeFlasher_Other=False 
    PharaohBullseyeFlash True

	TrooperDisable True 
'    PharaohBullseyeFlasherEnabled True
End Sub

Sub StartCyborg(bSkipUnderworldExit)
WriteToLog "     ", "StartCyborg"

	if IsModeQual(kModeCyborg) then exit sub 		' Just in case we aready started

	DisableTombTreasure

	QueueFlush 0
	QueueSkip 0		' Cancel currently running clip

	QueueScene2 0, "SceneGeneralStartDef False, False, ""CyborgMultiball"", ""Start1.mp4"", ""^^^^^^^^^"" ", 4000, 1, True 
	QueueScene2 0, "PlaySoundVol ""vo_cyborgmultiball"", VolDef:PuPlayer.LabelSet pDMDFull,""MsgFull"" ,""CyborgMultiball\\txtMB.png"", 1,""{'mt':2,'height':100,'width':100}"" ", 1000, 1, True
	QueueScene2 0, "SceneClearLabels: tmrCyborgFlash.Enabled=True", 0, 1, True
	QueueScene2 0, "EnableBallSaver 25:AddMultiball 2", 0, 1, True
	if bSkipUnderworldExit=False then QueueScene2 0, "Exit_Underworld False ", 0, 1, True
	QueueScene2 0, "PlaySoundAt""fx_SolenoidOff"", gion_bulbs_backwall: post001_IsDropped(1):post002_IsDropped(1)",0, 1, True

	StopEddieMode					' Stop Eddie Mode progress if it is running

    DMD CL(0, "CYBORG"), "     MULTIBALL", "", eNone, eBlink, eNone, 1500, True, ""
	PlaySoundVol "vo_wherearewe", VolDef
	SelectModeMusic kModeCyborg

	pClearPowerFeatures				' Clear all the PowerFeature Info

	EnableMummy False
	CheckMummyMBSave(kModeCyborg)							' Disable MummyMB and Save the state 

	AddScoreMode kModeCyborg, 500000						' Default Mode Points if nothing is hit 
    light045.State = 0				' Orb Jackpot 
	' Set Main Triange Inserts
    SetFastPulse(light014):light014.State = 0				' Red "?" Light at the top of Pyramid inserts
	SetFastPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 2
    SetFastPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 2
    SetFastPulse(lPowerSpinner):SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2
    SetFastPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 2
    SetFastPulse(lPowerRamps):SSetLightColor kModeMISC, kLightPowerRamps, noColor, 2

	' Update all mode lights
	SSetLightColor kModeCyborg, kLightRampRight,	yellow, 0
	SSetLightColor kModeCyborg, kLightRampCenter, 	yellow, 0
	SSetLightColor kModeCyborg, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeCyborg, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeCyborg, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeCyborg, kLightOrbitRight, 	yellow, 0

	SetLightColor lRampCenter, red, 0
	SetDefPulse lRampCenter
	lBattle.state = 0
	lLock.state = 0

	' Set Power Triange inserts around PF
    lPowerSpinnerArrow1.State = 2
    lOrbitArrow.State = 2
    lPowerSpinnerArrow2.State = 2
    lRampArrow.State = 2
    lOrbitArrow2.State = 2
	SetModeQual kModeCyborg, True 
	QueueSetDefault 0, "SceneCyborgWait", "SceneClearLabels"
	
	bCyborgSJPActivated=False

	' Flash Pup
	tmrCyborgFlash.Interval=200
	tmrCyborgFlash.UserValue=True 

	' Put post up to catch Multiballs
    post001_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
    post002_IsDropped(0)
  				
    LightEffect 5

	AddHudInfo kModeCyborg, "CYBORG", "MULTIBALL", "", "", True

End Sub

Sub CheckCyborg 'pops, spinners, orbits, ramps and targets must be hit 1 to activate SJP at ORB
	Dim JPtxt 
	Dim Multiplier

    'can just check the lights :)
    CyborgSJPValue(Currentplayer) = CyborgSJPValue(Currentplayer) + 25000 * CyborgDifficulty(CurrentPlayer)
    If bCyborgSJPActivated=False and GetLightState(kModeMISC, kLightPowerPops) = 1 AND GetLightState(kModeMISC, kLightPowerTargets) = 1 AND _
		GetLightState(kModeMISC, kLightPowerSpinners) = 1 AND GetLightState(kModeMISC, kLightPowerOrbits) = 1 AND _
		GetLightState(kModeMISC, kLightPowerRamps)= 1 Then

		light014.State = 2		' Red "?" Light at the top of Pyramid inserts
        light045.State = 2		' Light Jackpot
        bCyborgSJPActivated = True
    End If

	if IsModeQual(kModeCyborg) then 
		if CyborgSJPMultiplier<>1 then Multiplier=" x " & CyborgSJPMultiplier 
		PuPlayer.LabelSet pDMDFull,"Msg9", FormatScore(CyborgSJPValue(Currentplayer)) & Multiplier, 1,""
	End if 

End Sub

Sub AwardCyborgSJP
    Dim tmp
	Dim JPtxt

'    AddBonusMultiplier 1
    tmp = CyborgSJPValue(Currentplayer) * CyborgSJPMultiplier
	JPtxt=FormatScore(tmp)

	PlaySoundVol "vo_CyborgJP", VolDef
    DMD CL(0, JPtxt), "CYBORG SUPER JACKPOT", "", eNone, eBlinkFast, eNone, 1500, True, ""

    bCyborgSJPActivated = False
    light045.State = 0
	Light014.state = 0
	SSetLightColor kModeMISC, kLightPowerPops, noColor, 2
    SSetLightColor kModeMISC, kLightPowerTargets, noColor, 2
    SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2
    SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 2
    SSetLightColor kModeMISC, kLightPowerRamps, noColor, 2

	' Set Power Triange inserts around PF
    lPowerSpinnerArrow1.State = 2
    lOrbitArrow.State = 2
    lPowerSpinnerArrow2.State = 2
    lRampArrow.State = 2
    lOrbitArrow2.State = 2

    CyborgDifficulty(CurrentPlayer) = CyborgDifficulty(CurrentPlayer) + 0.5

	LightEffect 3

	QueueFlush 0
	QueueScene "SceneGeneralStart pDMDFull, False, False, ""CyborgMultiball"", ""JP"& INT(RND*3)+1 & ".mp4"", ""I:CyborgMultiball\\txtJP1.png^^^^^^" & JPtxt & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3034, 1
	QueueScene "SceneClearLabels", 0, 1
	AddScoreMode kModeCyborg, tmp

    CyborgSJPMultiplier = CyborgSJPMultiplier + 1
    CyborgSJPCount = CyborgSJPCount + 1

    If CyborgSJPCount<3 Then 		' Add Extra Ball First 2 times 
		AddMultiball 1
		EnableBallSaver 10
	End if 

	if CyborgSJPMultiplier=5 then
		AddJP cPowerFeatureValue, 5000000
		AddEddieCard kModeCyborg, True 
	End if 

End Sub

Sub StopCyborg
WriteToLog "     ", "STOP Cyborg "
	if IsModeQual (kModeCyborg)=False then		' We didnt really start  
'		SetModeActive kModeCyborg, False 
		SetLightColor lRampCenter, red, 0
		SetDefPulse light014:light014.state = 0	' Cyborg ? Light
		SetDefPulse light072:light072.state = 0	' Lightning Left 
		SetDefPulse light073:light073.state = 0	' Lightning Right 
		PharaohBullseyeFlasherEnabled False
		Exit Sub
	End if

	tmrCyborgFlash.Enabled=False
	puPlayer.LabelSet pDMDFull,"pJackpot","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':47.2,'ypos':27.2,'zback':1}"

    bCyborgSJPActivated = False
	EnableMummy True
	TrooperDisable False

	SetDefPulse(light014):light014.State = 0				' Red "?" Light at the top of Pyramid inserts
	SetFastPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 0
    SetFastPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 0
    SetFastPulse(lPowerSpinner):SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 0
    SetFastPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 0
    SetFastPulse(lPowerRamps):SSetLightColor kModeMISC, kLightPowerRamps, noColor, 0

	' Set Power Triange inserts around PF
    lPowerSpinnerArrow1.State = 0
    lOrbitArrow.State = 0
    lPowerSpinnerArrow2.State = 0
    lRampArrow.State = 0
    lOrbitArrow2.State = 0

	light072.State = 0
	light073.State = 0
    lUnderworld.State = 0
    lRampCenter.State = 0
    light045.State = 0				' Jackpot 
    PharaohBullseyeFlasherEnabled False

	' Reset PowerFeatures 
	PowerSpinnersCount(CurrentPlayer) = 0
	PowerPopsCount(CurrentPlayer)	= 0
	PowerRampsCount(CurrentPlayer)	= 0
	PowerTargetsCount(CurrentPlayer)= 0
	PowerOrbitsCount(CurrentPlayer)	= 0

	bPowerPops2(CurrentPlayer)=0
	bPowerSpinners2(CurrentPlayer)=0
	bPowerRamps2(CurrentPlayer)=0
	bPowerTargets2(CurrentPlayer)=0
	bPowerOrbits2(CurrentPlayer)=0
	' End Reset Power Features 

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeCyborg, False
	SetModeQual kModeCyborg, False

	UpdatePowerFeature						' Show the stats again

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""CyborgMultiball"", ""Total.mp4"", ""I:CyborgMultiball\\txtTotal.png^^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeCyborg)) & "^^^"", ""^^^^^^5000:" & pupColorRed & "^^^^"" ", 5000, 1
	QueueScene "SceneClearLabels", 0, 1

	RemoveHudInfo kModeCyborg
'	vpmtimer.addtimer 4000,"ModeEnd kModeCyborg '"
	ModeEnd kModeCyborg
	QueueScene2 0,"ModeEnd2 kModeCyborg", 10, 1, True
End Sub


Sub HideActiveModes()		' Hides active modes that persist on the screen 
	bActiveModeDisabled=True 	' Set this so other modes can detect and skip things if needed
	if IsModeQual(kModeCyborg) then 
		tmrCyborgFlash.Enabled=False
		puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':52,'ypos':45,'zback':1}"
		puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':40,'ypos':45,'zback':1}"
		puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':49,'ypos':35,'zback':1}"
		puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':46,'ypos':45,'zback':1}"
		puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':42.5,'ypos':35,'zback':1}"
		puPlayer.LabelSet pDMDFull,"pJackpot","PuPOverlays\\clear.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':46,'ypos':25,'zback':1}"
	End if
End Sub 
Sub ShowActiveModes()
	bActiveModeDisabled=False 
	if IsModeQual(kModeCyborg) then 
		tmrCyborgFlash.Enabled=True 
	End if 
End Sub 


Sub tmrCyborgFlash_Timer		' Update the Cyborg Pup
	tmrCyborgFlash.UserValue=tmrCyborgFlash.UserValue=False	' Toggle

' SPINNER
	if GetLightState(kModeMISC, kLightPowerSpinners)=1 or tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':52,'ypos':45,'zback':1}"
	Else 
		puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':53.2,'ypos':47.2,'zback':1}"
	End if

' POPS
	if GetLightState(kModeMISC, kLightPowerPops)=1 or tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':40,'ypos':45,'zback':1}"
	Else 
		puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':41.2,'ypos':47.2,'zback':1}"
	End if 

' RAMPS 
	if GetLightState(kModeMISC, kLightPowerRamps)=1 or tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':49,'ypos':35,'zback':1}"
	Else 
		puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':50.2,'ypos':37.2,'zback':1}"
	End if 

' TARGETS 
	if GetLightState(kModeMISC, kLightPowerTargets)=1 or tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':46,'ypos':45,'zback':1}"
	Else
		puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':47.2,'ypos':47.2,'zback':1}"
	End if 

' ORBITS 
	if GetLightState(kModeMISC, kLightPowerOrbits)=1 or tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':42.5,'ypos':35,'zback':1}"
	Else 
		puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':43.7,'ypos':37.2,'zback':1}"
	End if 

' Jackpot 
	if bCyborgSJPActivated=True and tmrCyborgFlash.UserValue then 
		puPlayer.LabelSet pDMDFull,"pJackpot","PuPOverlays\\PF-JP-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':46,'ypos':25,'zback':1}"
	Else 
		puPlayer.LabelSet pDMDFull,"pJackpot","PuPOverlays\\PF-JP-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':47.2,'ypos':27.2,'zback':1}"
	End if 

End Sub 



'*********************
'  EDDIE CARD LIGHTS
'*********************
Sub AddEddieCard(mode, bLevel2)
	Dim tmp
	Dim CP:CP=CurrentPlayer
	Dim EddieCode
	Dim EddieClip
	dim idx
	Dim calloutIdx
	Dim CalloutOffset
	if mode=kModeMummy then idx=0
	if mode=kModeCyborg then idx=1
	if mode=kModeTrooper then idx=2
	if mode=kMode2M2M then idx=3

'WriteToLog "     ", "AddEddieCard:" & Mode & " " & bLevel2 & " " & EddieCard(CurrentPlayer, idx)

	if bLevel2 then 
		if EddieCard(CurrentPlayer, idx)=2 then exit sub 	' Dont award twice 
		EddieCard(CurrentPlayer, idx)=2

	elseif EddieCard(CurrentPlayer, idx)=0 then  	' Dont go backwards 
		EddieCard(CurrentPlayer, idx)=1
	else 
		Exit sub									' Dont award twice
	End if 

	' Play Callout 
	calloutIdx=INT(RND*2)+1		' 1=Male, 2=Female
	CalloutOffset=0
	Select case Mode
		Case kModeMummy:
			PlaySoundVol "vo_ColMummy" & calloutIdx, VolDef
			if calloutIdx=1 then CalloutOffset=454
		case kModeCyborg:
			PlaySoundVol "vo_ColCyborg" & calloutIdx, VolDef
			if calloutIdx=1 then CalloutOffset=289
		case kModeTrooper:
			PlaySoundVol "vo_ColTrooper" & calloutIdx, VolDef
		case kMode2M2M:
			PlaySoundVol "vo_ColSoldier" & calloutIdx, VolDef
	End Select 

	if bLevel2 then 	' TBD Add LEVEL 2 callout (https://youtu.be/eXNu1O-vHWU?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=3069)
		VpmTimer.AddTimer 2203+CalloutOffset, "PlaySoundVol ""vo_ColLevel" & calloutIdx & """, VolDef '"
	End if
	
	lCardMummy.State=EddieCard(CurrentPlayer,0)
	lCardCyborg.State=EddieCard(CurrentPlayer,1)
	lCardTrooper.State=EddieCard(CurrentPlayer,2)
	lCard2M2M.State=EddieCard(CurrentPlayer,3)
	SetSlowPulse(lCardMummy)
	SetSlowPulse(lCardCyborg)
	SetSlowPulse(lCardTrooper)
	SetSlowPulse(lCard2M2M)

	tmp="" & EddieCard(CP, 0) & EddieCard(CP, 1) & EddieCard(CP, 2) & EddieCard(CP, 3)
	EddieCode=Replace(tmp, "2", "1")

	tmp="""fx_fire" & INT(RND*2)+1 & """ "
	QueueScene "PlaySoundVol " & tmp & ", VolDef", 0, 1
	QueueScene "SceneGeneralStartDef False, False, ""EDDIECards"", """&EddieCode&".mp4"", ""^^^^^^^^^"" ", 5000, 1
	QueueScene "StopSound " & tmp & ":SceneClearLabels", 0, 1

	' Tomb Award for Completing all Level 2 Eddie Cards
	if EddieCard(CurrentPlayer,0)=2 and EddieCard(CurrentPlayer,1)=2 and EddieCard(CurrentPlayer,2)=2 and EddieCard(CurrentPlayer,3)=2 then  
		StartTombTreasure(kTombEddie)
	End if 

	' Qualify NumberofTheBeast 
	if EddieCard(CurrentPlayer,0)<>0 and EddieCard(CurrentPlayer,1)<>0 and EddieCard(CurrentPlayer,2)<>0 and EddieCard(CurrentPlayer,3)<>0 then  
		If IsModeActive(kModeNOTB)=False then 
			EnableNOTB()
		End if 
	End if 

	EDDIESCollectedBonusCount = EDDIESCollectedBonusCount + 1

End Sub 

Sub UpdateEddieCards
	lCardMummy.State=EddieCard(CurrentPlayer,0)
	lCardCyborg.State=EddieCard(CurrentPlayer,1)
	lCardTrooper.State=EddieCard(CurrentPlayer,2)
	lCard2M2M.State=EddieCard(CurrentPlayer,3)
	SetSlowPulse(lCardMummy)
	SetSlowPulse(lCardCyborg)
	SetSlowPulse(lCardTrooper)
	SetSlowPulse(lCard2M2M)
End Sub



'******************
'   LOOP feature
'******************
' available during all modes
' shoot loops with upper flippers
' LoopValue = 250000
' LoopJackpot = 1000000
' LoopCount = 0

Sub LoopTimer_Timer 'decreases the LoopValue and after 8 seconds is back to 250k
	LoopTimer.UserValue=LoopTimer.UserValue+1
'WriteToLog "     ", "LoopTimer_Timer:" & LoopTimer.UserValue & " LoopCount:" & LoopCount

	If LoopCount = 7 then 	' JP Collect done 
		if LoopTimer.UserValue=10 then
			SetFastPulse Light021
			SetFastPulse Light022
		elseif LoopTimer.UserValue=12 then
			tmrLoopAnimate.Enabled = False
			LoopTimer.Enabled = False
			LoopCount=0
			LoopTimer.UserValue=0
			EndLoopJP
			pJackpotCounts False 
			UpdateLoopLights
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""wickerman5.mp4"", ""^^^^LOOP JACKPOTS TOTAL^^" & FormatScore(LoopJPTotal) & "^^^"", ""^^^^^^2000:" & pupColorRed & "^^^"" ", 2000, 1

		End if 
	elseif LoopCount = 6 then	' Cycle lights to start JP
		if LoopTimer.UserValue=8 then 
			LoopTimer.UserValue=0
			LoopAnimate
		End If
	else 							' Decrease Loop
		if LoopTimer.UserValue=8 then 
			LoopCount = LoopCount -1
			LoopTimer.UserValue=0
			Loop2x=False
			UpdateLoopLights
			pJackpotCounts False
			IF LoopCount = 0 Then Me.Enabled = 0
		elseif LoopTimer.UserValue=4 then
			UpdateLoopLights
		End if 
	End if 
End Sub

Sub LoopAnimate()	' Cycle Loop Lights once 
	tmrLoopAnimate.Enabled = False 
	tmrLoopAnimate.UserValue=0
	tmrLoopAnimate.interval = 60
	tmrLoopAnimate.Enabled = True
End Sub 

Sub tmrLoopAnimate_timer 
	tmrLoopAnimate.UserValue=tmrLoopAnimate.UserValue+1
	select case tmrLoopAnimate.UserValue
		case 0:Light040.state=1:Light041.state=0:Light042.state=0:Light043.state=0:Light020.state=0
		case 1:Light040.state=1:Light041.state=1:Light042.state=0:Light043.state=0:Light020.state=0
		case 2:Light040.state=0:Light041.state=1:Light042.state=0:Light043.state=0:Light020.state=0
		case 3:Light040.state=0:Light041.state=1:Light042.state=1:Light043.state=0:Light020.state=0
		case 4:Light040.state=0:Light041.state=0:Light042.state=1:Light043.state=0:Light020.state=0
		case 5:Light040.state=0:Light041.state=0:Light042.state=1:Light043.state=1:Light020.state=0
		case 6:Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=1:Light020.state=0
		case 7:Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=1:Light020.state=1
		case 8:Light040.state=0:Light041.state=0:Light042.state=0:Light043.state=0:Light020.state=1
	End Select 
	if tmrLoopAnimate.UserValue=8 then 
		tmrLoopAnimate.UserValue=0
		tmrLoopAnimate.Enabled = False
	End if 
End Sub 

'Sub LoopJackpotTimer_Timer 'turns off the loop jackpots after 12 seconds
'	EndLoopJP
'    Me.Enabled = 0
'End Sub

Sub EndLoopJP
	if light021.State <>0 then 
		LoopJackpotQual(CurrentPlayer) = LoopJackpotQual(CurrentPlayer)+1
		LoopJackpot(CurrentPlayer)=0
	End if 
    light021.State = 0
    light022.State = 0
End Sub 

Function GetLoopJPMax()
	Dim cnt:cnt=0
	Dim i 
	For i = 0 to LoopJackpotQual(CurrentPlayer)
		cnt=cnt+(5+(i*2))
	Next 
	GetLoopJPMax=cnt
End Function

Sub UpdateLoopLights
WriteToLog "     ", "UpdateLoopLights:" & LoopCount
	dim current:set current=nothing
    Select case LoopCount
        Case 0:light040.State = 1:light041.State = 0:light042.State = 0:light043.State = 0:light020.State = 0
        Case 1:light040.State = 1:light041.State = 2:light042.State = 0:light043.State = 0:light020.State = 0:set current=light041
        Case 2:light040.State = 1:light041.State = 1:light042.State = 2:light043.State = 0:light020.State = 0:set current=light042
        Case 3:light040.State = 1:light041.State = 1:light042.State = 1:light043.State = 2:light020.State = 0:set current=light043
        Case 4:light040.State = 1:light041.State = 1:light042.State = 1:light043.State = 1:light020.State = 2
        Case 5:light040.State = 1:light041.State = 1:light042.State = 1:light043.State = 1:light020.State = 1
    End Select

	if Loop2x then 
		light020.State = 1
	end if 

	if not current is Nothing then 
		if LoopTimer.UserValue>=4 then 
			SetFastPulse current
		else 
			SetDefPulse current
		End if 
	End if 
End Sub


Sub pJackpotCounts(bLoopAwarded)	' Updates HUD on the Right for Loops & Power JP
	If IsModeQual(kModeNOTB) then Exit Sub 

	' LOOPS 
	if Light021.state <>0 or bLoopAwarded then 
		PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsNeeded","LOOP AWARDED",1,"{'mt':2, 'xpos':93.5,'ypos':55, 'size':2.2}"
		PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsCount", LoopJPCount,1, "{'mt':2, 'xpos':93.5,'ypos':59, 'size':2.5}"	
	else 
		PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsNeeded","LOOPS NEEDED",1,"{'mt':2, 'xpos':93.5,'ypos':55, 'size':2.2}"
		PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsCount",LoopJackpotMulti(CurrentPlayer) & "/" & GetLoopJPMax(),1,"{'mt':2, 'xpos':93.5,'ypos':59, 'size':2.5}"
	End if 
	PuPlayer.LabelSet pDMDFull,"LoopJackpotLabelScore","LOOP JACKPOT",1, "{'mt':2, 'xpos':93.5,'ypos':63, 'size':2.2}" 'LoopJackpot total	
	PuPlayer.LabelSet pDMDFull,"LoopJackpotScore", FormatScore(LoopJackpot(CurrentPlayer)),1, "{'mt':2, 'xpos':93.5,'ypos':67, 'size':2.5}" 'LoopJackpot total	

	' JACKPOT 
	Puplayer.LabelSet pDMDFull,"Power","Power"    ,1,"{'mt':2, 'xpos':87,'ypos':79, 'size':2.5}"
	Puplayer.LabelSet pDMDFull,"Jackpot","Jackpot",1,"{'mt':2, 'xpos':87,'ypos':83, 'size':2.5}"
	Puplayer.LabelSet pDMDFull,"PowerJackpotMultiplier",PowerJackpotMultiplier &"X",1,"{'mt':2, 'xpos':100,'ypos':81.5, 'size':5}"  'PowerJackpot Multiplier
	PuPlayer.LabelSet pDMDFull,"PowerJackPotScore",FormatScore(PowerFeatureValue),1,"{'mt':2, 'xpos':94,'ypos':88.5, 'size':3}" 'Power Jackpot Total
end Sub


Sub pSplashLoopCount
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCount","LOOPS X " & LoopsBonusCount,1,"" ' Andrew
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCountScore",(LoopsBonusCount*250000),1,"" ' Andrew
End Sub

'*******************
'      COMBOS
'*******************
' don't time out (unlike in the Icarus Land ramp mode)
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as combos:
'	Left Ramp	0
'	Big Loop	1
'	Left Orbit	2
'	Mini Loop	3
'	Right Ramp	4
'	Right Orbit	5
' 6 targets up to a max of 5 combos

Sub CheckCombo(n)
	dim ComboScore 
	Dim Prefix:Prefix=""

	if isModeActive(kModeIcarus) then Exit Sub 'no combos during Icarus

    If Combo(n) = 1 Then                       'repeated shot stops the combo
        StopCombo
        Exit Sub
    End If

    Combo(n) = 1
	ComboCount=ComboCount+1
	ComboTotal(CurrentPlayer)=ComboTotal(CurrentPlayer)+1

	if ComboCount=2 then 
		bComboActivated = True
	End if 

	if bComboActivated then 

		' Start Death Blow timer for 4 seconds 
		tmrDeathBlow.Enabled = False 
		FlashFlasherSetSpeed 1, 2
		bFlasher1Enabled=True
		tmrDeathBlow.Interval = 4000 
		tmrDeathBlow.Enabled = True 

		' Play Sequence with Score
		select case ComboCount
			case 2: ComboScore=500000
			case 3: ComboScore=1000000
			case 4: ComboScore=4000000
			case 5: ComboScore=10000000
			case 6: ComboScore=20000000
		End Select 

		if bSuperCombo then 
			ComboScore=ComboScore*5
			Prefix="S"
		End if 

		PlaySoundVol "fx_guitar" & ComboCount, VolDef
		DMD "", "", "DMD_combo" & ComboCount & "x", eNone, eNone, eBlink, 1500, True, ""
		QueueScene "SceneGeneralStartDef False, False, ""Combo"", """ & Prefix & "Combo" & ComboCount & ".mp4"", ""^^^^^^" & FormatScore(ComboScore) & "^^^"" ", 1920, 1		
		AddScore ComboScore

		if ComboCount=6 then 
			ComboCompletions(CurrentPlayer)=ComboCompletions(CurrentPlayer)+1
			StartTombTreasure(kTombCombo)
		End if 
	End if 

End Sub

Sub StopComboPop()		' You get 1 pop during combo without cancelling it
	if ComboPopCount=0 then 
		ComboPopCount=1
		Exit Sub 
	Else
		StopCombo
	End if 
End Sub 


Sub StopCombo
	tmrDeathBlow.Enabled=False 
    bComboActivated = False
    ComboCount = 0
	ComboPopCount=0
    For X = 0 to 5
        Combo(x) = 0
    Next
    bFlasher1Enabled=False
End Sub

'*******************
'     Deathblow
'*******************
' end a combo with a special shot to award a deathblow
' posible targets
'	1 - Skillshot Target
'   2 - ORB target
'	3 - Mummy Captive Ball
'	4 - Bulls-eye/Pharoah target (SUPER)
'	5 - SJP Target (SUPER)
'   6 - Revive spinner (SUPER)
	
Sub CheckDeathblow(n)
	Dim DBScore
WriteToLog "     ", "CheckDeathblow:" & bComboActivated & " " & tmrDeathBlow.Enabled & " Idx:" & n & " ComboCount:" & ComboCount
    If bComboActivated and tmrDeathBlow.Enabled Then	' Combo is active and we are withing the 4 seconds from last shot 

		if n=4 and BullseyeMultiplier<>3 then Exit Sub		' Must be bullseye

        DeathblowBonusCount = DeathblowBonusCount + 1

		' Score is active combo score 
		select case ComboCount
			case 2: DBScore=500000
			case 3: DBScore=1000000
			case 4: DBScore=4000000
			case 5: DBScore=10000000
			case 6: DBScore=20000000
		End Select 

		if bSuperCombo then 
			DBScore=DBScore*5
		End if 

        Select case n
            case 1, 2, 3 'normal deathblow
				DBScore=500000
				PlaySoundVol "vo_deathblow", VolDef
                DMD "", "", "DMD_Death", eNone, eNone, eBlink, 1500, True, ""

				QueueScene "SceneGeneralStartDef False, False, ""Callouts"", ""DeathBlow.mp4"", ""^^7:DEATH BLOW^^7:" & FormatScore(DBScore) & "^^^^^"" ", 1867, 1		
                DMD "", CL(1, FormatScore(DBScore)), "", eNone, eBlink, eNone, 1500, True, ""
                AddScore DBScore  
            case 4, 5, 6 'super deathblow
				if ComboCount=3 then 		' Must have 3 combos to enable Super Death Blow
					DBScore=DBScore*3
					PlaySoundVol "vo_superdeathblow", VolDef
					DMD "", "", "DMD_Superd", eNone, eNone, eBlink, 1500, True, ""

					QueueScene "SceneGeneralStartDef False, False, ""Callouts"", ""DeathBlow.mp4"", ""^^7:SUPER DEATH BLOW^^7:" & FormatScore(DBScore) & " X 3^^" & FormatScore(DBScore)*3 & "^^^"" ", 1867, 1		
					DMD "", CL(1, FormatScore(ComboValue * ComboCount * 3)), "", eNone, eBlink, eNone, 1500, True, ""
					AddScore ComboValue * ComboCount * 3

				End if 
        End Select
		StopCombo
    End If
End Sub

Sub tmrDeathBlow_Timer()
	FlashFlasherSetSpeed 1, 0
End Sub 

'*******************
'  Extra Bonus Subs
'*******************

Sub aBonusSwitches_Hit(idx)
    SwitchBonusCount = SwitchBonusCount + 1
End Sub

'*********************
'   Tomb Treasures
'*********************
' DAP - Collect Soul Shard Example -> https://youtu.be/eXNu1O-vHWU?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1755
'
' Collecting Super Jackpot hits, spinner hits and other shots will turn on a Tomb Treasure
' collect at the left ramp
'1.	15 Million - ANDREW if you have triple playfield enabled then this should get you 45 Million
'2.	Super Slings (slingshots worth 250k + 1K increment for rest of ball)
'3.	Adds additional 15M to current Super Pyramid Jackpot and starts !! Feliz Navidad Cabrones !!
'4.	Super Combos (5x combos and deathblows for rest of ball)
'5.	Light Extra Ball
'6.	Light Ball Save.
'7.	Collect 2X Bonus - This 2X is multiplied by the current PF multiplier AND your current bonus multiplier so very large collects are possible here!
'8.	+5x Super Pyramid Jackpot multiplier
'9.	50 Million
'10.Start Run To The Hills


Const kTombShardFOD		=0
Const kTombShardIcarus	=1
Const kTombShardAces	=2
Const kTombShardRime	=3
Const kTombShardHallowed=4
Const kTombLoopJP		=5
Const kTombCombo		=6
Const kTombMummySJP		=7
Const kTombTrooperSJP	=8
Const kTombNOTB			=9
Const kTombEddie		=10
Sub StartTombTreasure(kIndex)
	if TombTreasureObj(CurrentPlayer, kIndex)=False then 
		TombTreasureObj(CurrentPlayer, kIndex)=1
	End if 
End Sub

Sub CheckTombTreasure()		' Start tombtreasure back up if we are not in a mode or MB
	Dim i
WriteToLog "     ", "CheckTombTreasure:" & bMultiBallMode & " " & GetActiveModeAll()
	if bMultiBallMode=False and GetActiveModeAll()=-1 then							' No Mode or Multiball - Start the tomb treasure
		For i = 0 to 10
			if TombTreasureObj(CurrentPlayer, i)=1 then 
WriteToLog "     ", "Setting up Tomb Treasure"
				bTombTreasureReady(CurrentPlayer) = True
				PlaySoundVol "vo_CollectPharaoh", VolDef
				OpenRamp True 
'				SSetLightColor kModeMISC, kLightRampLeft, red, 2
				Exit For
			End if 
		Next 	
	End if
End Sub 

Sub DisableTombTreasure()
'	if IsModeQual(kModeMummy) then exit Sub 		' Mummy is enabled so we leave this alone ???
WriteToLog "     ", "DisableTombTreasure:" & bTombTreasureReady(CurrentPlayer)

	Dim btmpTombTreasureReady
	Dim i
	btmpTombTreasureReady=False 
	For i = 0 to 10
		if TombTreasureObj(CurrentPlayer, i)=1 then btmpTombTreasureReady=True 
	Next 
	if btmpTombTreasureReady then 
		bTombTreasureReady(CurrentPlayer) = False
		if IsModeActive(kModeMummy)=False then 					' Make sure mummy is disabled before we do this
'			SSetLightColor kModeMISC, kLightRampLeft, red, 0
			OpenRamp False
		End if 
	End if 
End Sub 

Sub AwardTombTreasure
	Dim Msg7:Msg7=""
	Dim Msg7Color:Msg7Color=""
	Dim i

	if bMultiBallMode or GetActiveModeAll()<>-1 then Exit Sub ' Cant award tomb treasure when a mode is active

	' Mark it as awarded 
	For i = 0 to 10
		if TombTreasureObj(CurrentPlayer, i)=1 then 
			TombTreasureObj(CurrentPlayer, i)=2
			Exit For
		End if 
	Next 

'    SetLamp 12, 0
	PlaySoundVol "vo_PharaohTombTreasure", VolDef
	SSetLightColor kModeMISC, kLightRampLeft, red, 0
    LightEffect 2
	Flashforms l165_spot, 3000, 300, 0			' Flash the spotlight 

    Select Case TombTreasureCount(CurrentPlayer)
        Case 0:
            DMD CL(0, "TOMB AWARD"), CL(1, "15 MILLION"), "", eNone, eBlink, eNone, 1500, True, ""
            Addscore 15000000 
			Msg7 = "6:" & FormatScore(15000000)
			Msg7Color = "2000:" & pupColorRed
        Case 1:
            DMD CL(0, "TOMB AWARD"), CL(1, "SUPER SLINGSHOTS"), "", eNone, eBlink, eNone, 1500, True, ""
			SuperSlingScore=250000
            bSuperSlings=True
        Case 2:
			If DMDFet(kDMDFet_MadnessEnabled) then 
				DMD CL(0, "TOMB AWARD"), CL(1, "PLAY WITH MADNESS"), "", eNone, eBlink, eNone, 1500, True, ""
				StartMadness
				AddScore 550000
			Else 
				DMD CL(0, "TOMB AWARD"), CL(1, "+15MILL SPJ PYRAMID"), "", eNone, eBlink, eNone, 1500, True, ""
				AddJP cPowerFeatureValue, 15000000
			End if 
        Case 3:
            DMD CL(0, "TOMB AWARD"), CL(1, "SUPER 5X COMBO"), "", eNone, eBlink, eNone, 1500, True, ""
            bSuperCombo = True
        Case 4:
            DMD CL(0, "TOMB AWARD"), CL(1, "EXTRA BALL IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
			LightExtraBall()
        Case 5:
            DMD CL(0, "TOMB AWARD"), CL(1, "BALL SAVE 60 SEC"), "", eNone, eBlink, eNone, 1500, True, ""
            EnableBallSaver 60
        Case 6:
            DMD CL(0, "TOMB AWARD"), CL(1, "COLLECT 2X BONUS"), "", eNone, eBlink, eNone, 1500, True, ""
            BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 2
'			StartPFM	- Is this the right animation for this ??
        Case 7:
            DMD CL(0, "TOMB AWARD"), CL(1, "POWER FEATURE 5X SPJ"), "", eNone, eBlink, eNone, 1500, True, ""   
            PowerJackpotMultiplier = PowerJackpotMultiplier + 5
        Case 8:
			DMD CL(0, "TOMB AWARD"), CL(1, "50 MILLION"), "", eNone, eBlink, eNone, 1500, True, ""
            Addscore 50000000
        Case 9:
            DMD CL(0, "TOMB AWARD"), CL(1, "RUN TO THE HILLS"), "", eNone, eBlink, eNone, 1500, True, ""
            StartRunToTheHills
    End Select

	QueueScene2 0, "SceneGeneralStart pDMDFull, False, False, ""Tomb Treasures"", ""TT.mp4"", ""I:Tomb Treasures\\txtTT" & TombTreasureCount(CurrentPlayer) +1 & ".png^^^^^^" & Msg7 & "^^^"", ""^^^^^^" & Msg7Color & "^^^"" ", 2000, 1, True
	if gRampPos = 3 then 									' Add time so we load ramp 
		QueueScene2 0, "SceneClearLabels", 5000, 1, True 
	else 
		QueueScene2 0, "SceneClearLabels", 1000, 1, True 
	End if 
	QueueScene2 0, "CheckTombTreasure ", 0, 1, True 

    TombTreasureCount(CurrentPlayer) = (TombTreasureCount(CurrentPlayer) + 1)MOD 10
    bTombTreasureReady(CurrentPlayer) = False
	'trgSarcGate_Hit									' See if we need to also handle MummyMB
End Sub

'*************************
' Can I Play With Madness
'*************************
' Mode 11
' 3rd Tomb Award
' 2 multiball mode
' 2 arrows lit red,
' shoot a red one it becomes green,
' shoot a green one it becomes red
' make all arrows green
' shoot the Bullseye for a Super Jackpot (which can be multiplied by 3 according to the strength of the hit)
' start over with more arrows


Sub SceneMadnessWait()
	SceneGeneralStart pDMDFull, True, False, "WizMadness", "Madness.mp4", "I:WizMadness\\imgMadness.png^^^^^^^^^", "^^^^^^^^^"
End Sub


Sub StartMadness
	Dim tmp
	dim i

	if IsModeQual(kModeCyborg) then exit sub 
	If IsModeActive(kModeCyborg) then 				' Waiting for Cyborg
		SetModeActive kModeCyborg, False			' Disable Cyborg
		CyborgSaveStart=True
	End if 

	PlaySoundVol "vo_madness" & INT(RND * 2)+1, VolDef

	QueueFlush 0
	QueueScene "SceneGeneralStartDef False, False, ""WizMadness"", ""Madness.mp4"", ""I:WizMadness\\txtMadness.png^^^^^^^^^"" ", 3000, 1
	QueueScene "SceneClearLabels", 0, 1

	if IsModeActive(kModeEddie)=False then lBattle.State=0				' Disable Eddie Ramp if it is open
	StopEddieMode														' Stop Eddie Mode progress if it is running

	RampUp																' Ramp Start Up
	EnableBallSaver 20

	EnableMummy False
	CheckMummyMBSave(kModeMadness)										' Disable MummyMB and Save the state 

	DMD "", "", "madness", eNone, eNone, eBlink, 1500, True, ""
	AddScoreMode kModeMadness, 1000000							' Default Mode Points if nothing is hit 

	SetModeActive kModeMadness, True
	QueueSetDefault 0, "SceneMadnessWait", "SceneClearLabels"
	SelectModeMusic kModeMadness

	' Update all mode lights
	SSetLightColor kModeMadness, kLightRampRight,	yellow, 0
	SSetLightColor kModeMadness, kLightRampCenter, 	yellow, 0
	SSetLightColor kModeMadness, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeMadness, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeMadness, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeMadness, kLightOrbitRight, 	yellow, 0

	MadnessShotCnt=1
	MadnessShotCntInit=1
	MadnessJPCnt=0
	For i = 0 to 7 														' Set the light states
		MadnessShots(i)=-1
		if i=2 then 		' kLightOrbitLeft (Make Random??)
			MadnessShots(i)=2
			SetMadnessLight i, MadnessShots(i)
		End if
	Next

    AddMultiball 1
    LightEffect 5
    post001_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
    post002_IsDropped(0)
    vpmtimer.addtimer 2500, "PlaySoundAt""fx_SolenoidOff"", gion_bulbs_backwall: post001_IsDropped(1):post002_IsDropped(1) '"
    CanIPlayWithMadnessStep = 0

	AddHudInfo kModeMadness, "CAN I PLAY", "WITH MADNESS", "", "", True

End Sub

Sub StopMadness
WriteToLog "     ", "Stop Madness"
	tmrMadnessLock.Enabled=False

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeMadness, False
	EnableMummy True 

	' Reset all lights
	SSetLightColor kModeMadness, kLightRampLeft, 	yellow, 0
	SSetLightColor kModeMadness, kLightRampRight, 	yellow, 0
	SSetLightColor kModeMadness, kLightLoopLeft, 	yellow, 0
	SSetLightColor kModeMadness, kLightLoopRight, 	yellow, 0
	SSetLightColor kModeMadness, kLightSpinnerLeft,yellow, 0
	SSetLightColor kModeMadness, kLightOrbitLeft, 	yellow, 0
	SSetLightColor kModeMadness, kLightOrbitRight, yellow, 0
	SSetLightColor kModeMadness, kLightCaptiveBall, yellow, 0

	RampDown		' Make sure ramp goes down

	QueueScene "SceneGeneralStart pDMDFull, False, False, ""WizMadness"", ""Madness.mp4"", ""I:WizMadness\\txtTotal.png^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeMadness)) & "^^^^"", ""^^^^^3000:" & pupColorRed & "^^^^^"" ", 3000, 1
	QueueScene "SceneClearLabels", 0, 1

	RemoveHudInfo kModeMadness
'	vpmtimer.addtimer 3000,"PupOverlayInGame:ModeEnd kModeMadness '"
	ModeEnd kModeMadness
	QueueScene2 0, "PupOverlayInGame:ModeEnd2 kModeMadness", 10,1,True
End Sub

'*************************
'   Run To The Hills
'*************************
'Mode 12 - Mega Wizard Mode
'10th Tomb Award
'6 ball multiball
'All shots are lit for jackpot

Sub SceneRTTHWait()
	SceneGeneralStart pDMDFull, True, False, "wizRTTH", "RTTH.mp4", "I:wizRTTH\\txtWait.png^^" & FormatScore(ModeJPPoints(CurrentPlayer, kModeRTTH)) &  "^^^^^^" & FormatScore(RTTH_SJP) &  "^", "^^" & pupColorRed & "^^^^^^" & pupColorRed & "^"
End Sub

Sub StartRunToTheHills

	PlaySoundVol "vo_run", VolDef

	QueueFlush 0
	QueueScene "SceneGeneralStartDef False, False, ""wizRTTH"", ""RTTH.mp4"", ""I:wizRTTH\\txtStart.png^^^^^^^^^"" ", 2000, 1
	QueueScene "SceneClearLabels", 0, 1

	if IsModeActive(kModeEddie)=False then lBattle.State=0				' Disable Eddie Ramp if it is open
	StopEddieMode														' Stop Eddie Mode progress if it is running

	pClearPowerFeatures													' Clear all the PowerFeature Info
	EnableBallSaver 45

    DMD "", "", "DMD_Run", eNone, eNone, eBlink, 1500, True, "" : pupevent 900
	AddScoreMode kModeRTTH, 500000										' Default Mode Points if nothing is hit 
	SetModeActive kModeRTTH, True
	QueueSetDefault 0, "SceneRTTHWait", "SceneClearLabels"
	SelectModeMusic kModeRTTH

	ModeJPPoints(CurrentPlayer, kModeRTTH)=5000000
	RTTH_SJP=0
	RTTH_JPX=1

	AddHudInfo kModeRTTH, "RUN TO", "THE HILLS", "", "", True

    AddMultiball 5
    LightEffect 5

    SetFastPulse(light014):light014.State = 0				' Red "?" Light at the top of Pyramid inserts
    SetFastPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 0
    SetFastPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 0
    SetFastPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 0
    SetFastPulse(lPowerRamps):SSetLightColor kModeMISC, kLightPowerRamps, noColor, 0

	' Set Power Triange inserts around PF
    lOrbitArrow.State = 0
    lRampArrow.State = 0
    lOrbitArrow2.State = 0

	SSetLightColor kModeRTTH, kLightRampLeft  , blue, 2 
	SSetLightColor kModeRTTH, kLightOrbitLeft , blue, 2
	SSetLightColor kModeRTTH, kLightLoopLeft  , blue, 2
	SSetLightColor kModeRTTH, kLightLoopRight , blue, 2
	SSetLightColor kModeRTTH, kLightRampRight , blue, 2
	SSetLightColor kModeRTTH, kLightOrbitRight, blue, 2
	SSetLightColor kModeRTTH, kLightSpinnerLeft, blue, 2

	' Spinners are first
	SetFastPulse(lPowerSpinner):SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2
    lPowerSpinnerArrow1.State = 2
    lPowerSpinnerArrow2.State = 2

End Sub

Sub StopRTTH()
WriteToLog "     ", "Stop RTTH"

'	QueueSetDefault 0, "", ""				' Disable for this scene 
	SetModeActive kModeRTTH, False

	' Reset Everything 
	SetFastPulse(light014):light014.State = 0				' Red "?" Light at the top of Pyramid inserts
	SetFastPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 0
	SetFastPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 0
	SetFastPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 0
	SetFastPulse(lPowerRamps):SSetLightColor kModeMISC, kLightPowerRamps, noColor, 0

	' Set Power Triange inserts around PF
	lOrbitArrow.State = 0
	lRampArrow.State = 0
	lOrbitArrow2.State = 0

	StopRainbow 
	lUnderworld.State = 0
	PharaohBullseyeFlasherEnabled False 

	QueueScene "SceneGeneralStart pDMDFull, True, False, ""WizRTTH"", ""RTTH.mp4"", ""I:WizRTTH\\txtTotal.png^^^^^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeRTTH)) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^^"" ", 3000, 1
	QueueScene "SceneClearLabels", 0, 1

	RemoveHudInfo kModeRTTH
'	vpmtimer.addtimer 3000,"PupOverlayInGame:ModeEnd kModeRTTH '"
	ModeEnd kModeRTTH
	QueueScene2 0, "PupOverlayInGame:ModeEnd2 kModeRTTH", 10,1,True

End Sub 

Sub CheckRunToTheHills                                                                                                                                                            'turn on Mummy SJP
    If lOrbitLeft.State = 0 AND lRampRight.State = 0 AND lLoopLeft.State = 0 AND lOrbitRight.State = 0 AND lRampLeft.State = 0 AND lLoopRight.State = 0 AND lRampCenter.State = 0 Then 'all 7 lights are off
        PharaohBullseyeFlasherEnabled True
        bRunToTheHillsSJPEnabled = True
    End If
End Sub


'ANDREW STARTED ADDING IN NEW NOTB MODE 13 BELOW


'*************************
'   Number Of The Beast
'*************************
' Untimed single-ball mode with a generous 30-second initial ball save
' Phase 1: three shots lit with red arrows. Shoot any one of them to light timed Bulls-eye shot.
' Phase 2: Shoot Bulls-eye within 5 seconds to start Counter-Attack phase, otherwise it reverts to Phase 1.
' Phase 3: Counter-Attack - all shots lit yellow for 10 seconds, but you cannot shoot the same shot consecutively.
' Defeat the Beast with 15 Counter-Attack shots. You can requalify and repeat the Counter-Attack phase as needed.
' If you lose your ball, you lose your battle against the Beast and have to requalify all 4 Eddie cards to challenge the Beast again (except for those Eddie cards that you collected a Level 2 Eddie card). 
' If you have all four Level 2 Eddie cards, you will immediately get to rematch the Beast if you have another ball.
' Defeating the Beast disables the flippers, drains your ball, then awards 100 Million plus re-awards the value of any Soul Shards collected earlier. 
' Normal gameplay resumes on your current ball in play, also with a fresh ball saver. Once the Beast has been defeated, it cannot be challenged again until after playing Run to the Hills.

Sub SceneNOTBWait()
	if NOTBPhase=1 then 
		SceneGeneralStartDef True, False, "NOTB", "wait1.mp4", "I:NOTB\\H" & NOTBAttackCount & ".png^^^^^^^^^"
	Elseif NOTBPhase=2 then
		SceneGeneralStartDef True, False, "NOTB", "wait2.mp4", "^^^^^^^^^"
	Elseif NOTBPhase=3 then
		SceneGeneralStartDef True, False, "NOTB", "wait3.mp4", "I:NOTB\\Y" & NOTBAttackCount & ".png^^^^^^^^^"
	End if 
End Sub 

'https://youtu.be/eXNu1O-vHWU?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=3283
'https://youtu.be/bzsM9HcUSW0?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1992
Sub StartNOTB()
	post001_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
	post002_IsDropped(0)

	UpdateForMultiball True 
	QueueFlush 0
	QueueSkip 0		' Cancel currently running clip
	QueueScene2 0, "SceneGeneralStartDef False, False, ""NOTB"", ""Start.mp4"", ""^^^^^^^^^"" ", 3000, 1, True 
	QueueScene2 0, "PlaySoundVol ""vo_notb"" & INT(RND * 2)+1, VolDef ", 2000, 1, True 
	QueueScene2 0, "PlaySoundVol ""sfx_explode1"", VolSfx ", 1400, 1, True 
	QueueScene2 0, "SceneGeneralStartDef False, False, ""NOTB"", ""Start2.mp4"", ""^^^^^^^^^"" ", 0, 1, True 
	QueueScene2 0, "PlaySoundVol ""sfx_notb_reload"", VolSfx ", 0, 1, True 
	QueueScene2 0, "PlaySoundVol ""vo_youunderestimatemypower"", VolDef ", 1534, 1, True 
	QueueScene2 0, "SceneClearLabels:SceneNOTBWait()", 500, 1, True 
	QueueScene2 0, "NOTBStart2", 1, 1, True 

	DMD "", "", "DMD_NOTB", eNone, eNone, eBlink, 1500, True, ""	
	AddScoreMode kModeNOTB, 500000						' Default Mode Points if nothing is hit 

	EnableMummy False
	CheckMummyMBSave(kModeNOTB)							' Disable MummyMB and Save the state 

	SetModeQual kModeNOTB, True

	QueueSetDefault 0, "SceneNOTBWait", ""

    LightEffect 5

'    post001.IsDropped = 1:PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
'    post002.IsDropped = 1
    EnableBallsaver 30

	ModeJPPoints(CurrentPlayer, kModeNOTB)=10000000

	SSetLightColor kModeNOTB, kLightRampLeft  , yellow, 0
	SSetLightColor kModeNOTB, kLightLoopRight , yellow, 0
	SSetLightColor kModeNOTB, kLightSpinnerLeft, yellow, 0
	SSetLightColor kModeNOTB, kLightOrbitLeft , yellow, 0
	SSetLightColor kModeNOTB, kLightRampRight, 	red, 2
	SSetLightColor kModeNOTB, kLightOrbitRight, red, 2
	SSetLightColor kModeNOTB, kLightLoopLeft,   red, 2

	bNOTB_Drained=False 
	bEndNOTB_Started=False 
	bNOTB_Finished = False
    NOTBPhase = 1 				'Start NOTB at Phase 1 - hit red arrow to Parry the attack
	NOTBAttackCount = 15		'Need to hit 15 yellow targets counting down to zero
	NOTBYellowAttackCount = 0	'Need to count yellow hits as the award value is based on this count

	pClearEverything
	pClearBonusTxt
	pClearJackpotCounts
	PupOverlayScore

End Sub

Sub NOTBStart2()
	PlaySoundAt "fx_SolenoidOff", gion_bulbs_backwall
	post001_IsDropped(1)
	post002_IsDropped(1)
End Sub 

Sub EnableNOTB()
	SelectModeMusic kModeNOTB
	SetModeActive kModeNOTB, True
	PlaySoundVol "vo_EnterTombDare", VolDef
End Sub 


Sub tmrNOTB_Timer
	Dim rndIdx
	Dim rndIdx2
	Dim rndIdx3

	tmrNOTB.UserValue = tmrNOTB.UserValue - 1 
    UpdateClock tmrNOTB.UserValue
    
	If tmrNOTB.UserValue = 3 Then 	' Flash fast when we get here 
		SetFastPulse lRampLeft
		SetFastPulse lOrbitLeft
		SetFastPulse lLoopLeft
		SetFastPulse lLoopRight
		SetFastPulse lOrbitRight
		SetFastPulse lRampRight

	elseIf tmrNOTB.UserValue = 0 Then 'reset everything back to phase one (3 red Lights)
		tmrNOTB.Enabled = False 
		NOTBPhase = 1
		PharaohBullseyeFlasherEnabled False

		SSetLightColor kModeNOTB,kLightRampLeft  , red, 0 ' Clear All
		SSetLightColor kModeNOTB,kLightOrbitLeft , red, 0
		SSetLightColor kModeNOTB,kLightLoopLeft  , red, 0
		SSetLightColor kModeNOTB,kLightRampCenter, red, 0
		SSetLightColor kModeNOTB,kLightLoopRight , red, 0
		SSetLightColor kModeNOTB,kLightRampRight , red, 0
		SSetLightColor kModeNOTB,kLightOrbitRight, red, 0
		SetDefPulse lRampLeft
		SetDefPulse lOrbitLeft
		SetDefPulse lLoopLeft
		SetDefPulse lLoopRight
		SetDefPulse lOrbitRight
		SetDefPulse lRampRight

		' Pick up to 3 to unlight based on modes complete 
		rndIdx = INT(RND*6)
		rndIdx2= INT(Rnd*6)
		rndIdx3= INT(Rnd*6)
		while rndIdx = rndIdx2
			rndIdx2=Int(Rnd*6)
		wend
		while rndIdx3 = rndIdx2 or rndIdx3 = rndIdx
			rndIdx3=Int(Rnd*6)
		wend
		if rndIdx=0 or rndIdx2=0 or rndIdx3=0 then SSetLightColor kModeNOTB,kLightRampLeft  , red, 2 ' Ramdomly Unlight up to 3 arrows
		if rndIdx=1 or rndIdx2=1 or rndIdx3=1 then SSetLightColor kModeNOTB,kLightOrbitLeft , red, 2
		if rndIdx=2 or rndIdx2=2 or rndIdx3=2 then SSetLightColor kModeNOTB,kLightLoopLeft	, red, 2
		if rndIdx=3 or rndIdx2=3 or rndIdx3=3 then SSetLightColor kModeNOTB,kLightLoopRight , red, 2
		if rndIdx=4 or rndIdx2=4 or rndIdx3=4 then SSetLightColor kModeNOTB,kLightRampRight , red, 2
		if rndIdx=5 or rndIdx2=5 or rndIdx3=5 then SSetLightColor kModeNOTB,kLightOrbitRight, red, 2

		QueueFlush 0 
		QueueScene2 0, "SceneGeneralStartDef False, False, ""NOTB"", ""Start2.mp4"", ""^^^^^^^^^"" ", 1, 1, True 
		QueueScene2 0, "PlaySoundVol ""sfx_notb_reload"", VolSfx ", 0, 1, True 
	End If
End Sub

Dim bNOTB_Finished
Dim bEndNOTB_Started
Dim bNOTB_Drained
Sub EndNOTB(bComplete)
WriteToLog "     ", "EndNOTB :" & bComplete & " bEndNOTB_Started:" & bEndNOTB_Started
	if bEndNOTB_Started=False then
		bEndNOTB_Started=True
		bNOTB_Finished=bComplete

		if bComplete then 
			DisableTable True
			bBallSaverActive = False
			QueueSetDefault 0, "", ""

			QueueFlush 0
			VolumeLower pMusic, 20		' Make it quiet
			QueueScene2 0, "SceneGeneralStartDef False, False, ""NOTB"", ""Finish.mp4"", ""^^^^^^^^^"" ", 1, 1, True 
			QueueScene2 0, "PlaySoundVol ""sfx_notb_end"", VolSfx ", 1, 1, True 
			QueueScene2 0, "PlaySoundVol ""vo_youdiditeddie"", VolDef ", 3000, 1, True 
			QueueScene2 0, "NOTBAwards", 1, 1, True
		Else 
			bNOTB_Drained=True
			QueueScene2 0, "SceneGeneralStart pDMDFull, False, False, ""NOTB"", ""fail.mp4"", ""I:NOTB\\txtTotal.png^^^^^^" & FormatScore(ModePoints(CurrentPlayer, kModeNOTB)) & "^^^"", ""^^^^^^5000:" & pupColorRed & "^^^"" ", 1, 1, True 
			QueueScene2 0, "PlaySoundVol ""vo_YesEmbraceFailure"", VolSfx ", 5000, 1, True 
			QueueScene2 0, "EndNOTB2:EndOfBall ", 0, 1, True 
		End if
	elseif bEndNOTB_Started and bNOTB_Finished and bComplete=False then 
		bNOTB_Drained=True
	End if

End Sub

Sub EndNOTB2()
WriteToLog "     ", "EndNOTB2"
	playclear pDMDFull
	DisableTable False
	tmrNOTB2.Enabled = False 
	UpdateForMultiball False 

	if bNOTB_Finished=False then  
		if EddieCard(CurrentPlayer,0)=2 and EddieCard(CurrentPlayer,1)=2 and EddieCard(CurrentPlayer,2)=2 and EddieCard(CurrentPlayer,3)=2 then 
			SetModeQual kModeNOTB, False 
		Else
			SetModeActive kModeNOTB, False 
			SetModeQual kModeNOTB, False 
			'  You have to earn level 1 cards again 
			if EddieCard(CurrentPlayer,0)=1 then EddieCard(CurrentPlayer,0)=0
			if EddieCard(CurrentPlayer,1)=1 then EddieCard(CurrentPlayer,1)=0
			if EddieCard(CurrentPlayer,2)=1 then EddieCard(CurrentPlayer,2)=0
			if EddieCard(CurrentPlayer,3)=1 then EddieCard(CurrentPlayer,3)=0
			UpdateEddieCards
		End if 
	Else 
		StartTombTreasure(kTombNOTB)
		SetModeActive kModeNOTB, False 
		SetModeQual kModeNOTB, False 
	End if 

	SSetLightColor kModeNOTB, kLightOrbitLeft , yellow, 0
	SSetLightColor kModeNOTB, kLightRampRight, 	red, 0
	SSetLightColor kModeNOTB, kLightOrbitRight, red, 0
	SSetLightColor kModeNOTB, kLightLoopLeft,   red, 0

	EnableMummy True 
	VolumeRestore pMusic, 1			' Restore Volume level

	SceneClearLabels
	pClearBonusTxt

	PupOverlayInGame

	QueueSetDefault 0, "", ""
	SetupEddieInserts
End Sub 


Sub NOTBAwards()
WriteToLog "     ", "NOTBAwards"
	QueueFlush 0
	playclear pDMDFull
	SceneGeneralStart pDMDFull, True, False, "NOTB", "Finish2.mp4", "^^^^^^^^^", "^^^^^^^^^"

	tmrNOTB2_Skip=False
	tmrNOTB2_Total=0
	tmrNOTB2.Interval = 1000
	tmrNOTB2.UserValue=0
	tmrNOTB2.Enabled = True 

End Sub 

Sub ShowNOTBAwardLine(BonusName, Title, Score)
	PuPlayer.LabelSet pDMDFull,"Lbl" & BonusName, Title, 1,""	
	PuPlayer.LabelSet pDMDFull,BonusName, FormatScore(Score), 1,""

	tmrNOTB2_Total=tmrNOTB2_Total+score
	PuPlayer.LabelSet pDMDFull,"BonusTotal", FormatScore(tmrNOTB2_Total), 1,""
End Sub 


Dim tmrNOTB2_Total
Dim tmrNOTB2_Skip
Sub tmrNOTB2_Timer
	tmrNOTB2.UserValue=tmrNOTB2.UserValue+1
	Select case tmrNOTB2.UserValue
		case 1:
			PlaySoundVol "fx_gunshot", VolSfx
			ShowNOTBAwardLine "BonusSwitches",			"Beast",	 					100000000
		case 2:
			PlaySoundVol "fx_gunshot", VolSfx
			ShowNOTBAwardLine "BonusMummyLetters",		"Aces High", 					SoulShardTotal(CurrentPlayer, kModeAces-2)
		Case 3: 
			PlaySoundVol "fx_gunshot", VolSfx
			ShowNOTBAwardLine "BonusDeathblows",		"Fear Of the Dark", 			SoulShardTotal(CurrentPlayer, kModeFear-2)
		Case 4:
			PlaySoundVol "fx_gunshot", VolSfx
			ShowNOTBAwardLine "BonusLoops",				"Rime of the Ancient Mariner", 	SoulShardTotal(CurrentPlayer, kModeRime-2)
		Case 5:
			PlaySoundVol "fx_gunshot", VolSfx
			ShowNOTBAwardLine "BonusPowerFeatures",		"Hallowed Be Thy Name", 		SoulShardTotal(CurrentPlayer, kModeHallowed-2)
		Case 6:
			PlaySoundVol "sfx_notb_total", VolSfx
			ShowNOTBAwardLine "BonusSoulShards",		"Flight Of Icarus", 			SoulShardTotal(CurrentPlayer, kModeIcarus-2)
		case 7:
			if bNOTB_Drained = False then		' Wait for the drain 
				tmrNOTB2.UserValue=6
			Else 
				AddScore tmrNOTB2_Total
				EndNOTB2
				Mode(CurrentPlayer, kMode2M2M) = 1
				lNOTB.state = 1
				QueueSetDefault 0, "UpdateEDDIELetter", ""
				AddMultiball 1
			End if 
	End Select 
	if tmrNOTB2_Skip then tmrNOTB2.Interval=1
End Sub 

Sub NOTBCallback	' FlipperSkip Callback
	' Flipper Skip
	if LFPress and RFPress then		' Skip to total
		if tmrNOTB2.Enabled then 
			tmrNOTB2.Enabled = False 
			tmrNOTB2.Interval = 1
			tmrNOTB2.Enabled = True
		End if 
	End if 
End Sub 


'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper	=0
Const pDMD		=1			' referenced from the Screen numbers set up in your PupPack
Const pBackglass=2
Const pPlayfield=3
Const pMusic	=4
Const pDMDFull	=5
Const pOverVid	=11
Const pTransp   =15
'Const pCallouts=6
'Const pBackglass2=7
'Const pTopper2=8
'Const pPopUP=9
'Const pPopUP2=10
'Const pModes=15

'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2



'Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode





'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub

Sub pDMDHighScore(msgText,msgText2,msgText3,msgText4,timeSec,mColor)'FOR HIGH SCORE
PuPlayer.LabelShowPage pDMD,5,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"   
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}" 
PuPlayer.LabelSet pDMD,"Splash7d",msgText4,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"  
end Sub

Sub pDMDSplash3Lines(msgText,msgText2,msgText3,timeSec,mColor)'FOR NORMAL DMD USE
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash2a",msgText,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"
PuPlayer.LabelSet pDMD,"Splash2b",msgText2,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"  
PuPlayer.LabelSet pDMD,"Splash2c",msgText3,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"  
end Sub

Sub pDMDBonus(msgText,msgText2,timeSec,mColor)'FOR BONUS
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}"
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,0,"{'mt':1,'at':2,'fq':250,'len':" & (timeSec*1000000) & ",'fc':" & mColor & "}" 
end Sub

Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)    
WriteToLog "     ", "pDMDSetPage:" & pagenum
    PuPlayer.LabelShowPage pDMDFull, pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
PuPlayer.LabelSet pDMD,"Splash7d",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
pDMDCurPriority=pPriority
if timeSec=0 then timeSec=1 'don't allow page default page by accident


pLine1=""
pLine2=""
pLine3=""
pLine1Ani=""
pLine2Ani=""
pLine3Ani=""


if pAni=1 Then  'we flashy now aren't we
pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
end If

curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if


'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False
End if


if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
    
    PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
    pDMDVideoPlaying=true
end if 'if showing a splash video with no text




if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message


Const cPowerJackpotMultiplier=0
Const cPowerFeatureValue=1
Const cLoopJackpotMulti=2
Sub AddJP(JPIndex, Value)			' Increments Counters (How many loops, Multipliers, etc)
	Select Case JPIndex 
		case 0: ' PowerJackpotMultiplier
			PowerJackpotMultiplier = PowerJackpotMultiplier + value
		case 1: 'PowerFeatureValue
			PowerFeatureValue = PowerFeatureValue + value 
		case 2: ' LoopJackpotMulti
			LoopJackpotMulti(CurrentPlayer) = LoopJackpotMulti(CurrentPlayer) + value 
	End Select 

	if bSongSelect=False then pJackpotCounts False
End Sub 


Sub UpdateDMDStats
	if bSongSelect then exit sub 
	pPowerFeatures
	pCountdownTimer
	pJackpotCounts False 
End Sub

Sub pupDMDupdate_Timer()

	pUpdateScores
	if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
'WriteToLog "     ", "PriorityReset=" & PriorityReset
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false			
		End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
		End if
    end if 
End Sub



'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

dim i
DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed
Dim serviceFont
Dim serviceFont2
Dim monoFont
Dim glyphsFont

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we'd set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.


'puPlayer.LabelNew pBackglass,"RampImg",digitLCD,	4,RGB(255, 255, 255)  					,0,0,0 ,0,38  ,1,0					' Rampage Countdown timer Image
'puPlayer.LabelSet pBackglass,"RampImg", "PuPOverlays\\Rampage.gif", 0,"{'mt':2,'color':111111,'width':15, 'height':24, 'anigif':100,'pagenum':1}"

PuPlayer.LabelInit pDMDFull
PuPlayer.LabelInit pOverVid
PuPlayer.LabelInit pTransp

if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  
	dmdalt="stern"
    dmdfixed="stern"
    dmdscr="stern"    'main scorefont
	dmddef="stern"

	'Page 1 (default score display)
  		 PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,100,33023   ,0,2,2,95,0,1,0
		 PuPlayer.LabelNew pDMD,"Player"   ,dmdscr,100,33023   ,1,0,0,15,0,1,0
		 PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,100,33023   ,1,2,0,85,0,1,0
		 PuPlayer.LabelNew pDMD,"MsgScore",dmdscr,100,33023   ,0,1,0, 0,40,1,0
		 PuPlayer.LabelNew pDMD,"CurScore",dmdscr,100,8454143   ,0,1,1, 0,68,1,0
		 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,70,33023,0,1,1,0,0,1,0
		


	'Page 2 (default Text Splash 1 Big Line)
		 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,10,33023,0,1,1,0,0,1,0

	'Page 3 (default Text Splash 2 and 3 Lines)
		 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
	     PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,55,3,0


	'Page 4 (2 Line Gameplay DMD)
		 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
	     PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

 	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmdscr,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdscr,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdscr,40,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver

if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd ************** 

	dmdalt="stern"    
    dmdfixed="stern"
	dmdscr="stern"  'main score font
	dmddef="stern"

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmddef,50,RGB(255,255,255),0,2,2,95,0,1,0
		PuPlayer.LabelNew pDMD,"Player"   ,dmdalt,50,RGB(255,255,255),0,0,0,15,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,50,RGB(255,255,255),0,2,0,85,0,1,0
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,50,RGB(255,255,255),0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,50,RGB(255,255,255),0,1,1, 0,68,1,0
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,70,RGB(255,255,255),0,1,1,0,0,1,0
	'Page 2 (default Text Splash 1 Big Line)
		

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,10,RGB(255,255,255),0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,10,RGB(255,255,255),0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,15,RGB(255,255,255),0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,10,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,10,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,10,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,10,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,10,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,10,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,10,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,10,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,10,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,10,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,10,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver

if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************  

	'dmddef="stern"
	dmdalt="ironmaiden"    
    dmdfixed="TwoMinToMid"
	dmdscr="stern"  'main score font
	dmddef="stern"
	serviceFont2="Gas"
	serviceFont="DotMatrix"
	monoFont="SternSystem Mono"    'main scorefont.  Good FontTool: https://fontforge.org/docs/tutorial/editexample2.html#creating-the-letter-o-consistent-directions
	glyphsFont="Ancient Egyptian Hieroglyphs"


'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'labelNew <screen#>, <"Labelname">, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (Default labels)
		PuPlayer.LabelNew pDMDFull,"Ball"               ,dmdscr,6,RGB(255,255,255),0,	0,0,	37,0	,1,0
		PuPlayer.LabelNew pDMDFull,"Credits"            ,dmdscr,6,RGB(255,255,255),0,	0,0,	49,0	,1,0
'		PuPlayer.LabelNew pDMDFull,"CurrentPlayer"      ,dmdscr,6,RGB(255,255,255),	0,	1,2,	17,99	,1,0
		PuPlayer.LabelNew pDMDFull,"CurrentPlayerScore" ,dmdscr,12,RGB(255,255,255),0,	1,2,	0,0		,1,0

		PuPlayer.LabelNew pDMDFull,"HUD1"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'HUD BG on Right  
		PuPlayer.LabelNew pDMDFull,"HUD2"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'HUD BG on Right 
		PuPlayer.LabelNew pDMDFull,"HUD3"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,0,	25,0	,1,0 'HUD BG on Right 
		PuPlayer.LabelNew pDMDFull,"HUD4"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,0,	25,0	,1,0 'HUD BG on Right 
		PuPlayer.LabelNew pDMDFull,"P1Bg"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'BG Image for Player Score 
		PuPlayer.LabelNew pDMDFull,"P2Bg"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'BG Image for Player Score 
		PuPlayer.LabelNew pDMDFull,"P3Bg"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'BG Image for Player Score 
		PuPlayer.LabelNew pDMDFull,"P4Bg"    		     ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'BG Image for Player Score 

		PuPlayer.LabelNew pDMDFull,"ScorePos1"          ,dmdscr,7,RGB(255,157,6),	0,	1,2,	25,0	,1,0 'Additional Plater Scores 
		PuPlayer.LabelNew pDMDFull,"ScorePos2"          ,dmdscr,7,RGB(255,157,6),	0,	1,2,	50,0	,1,0
		PuPlayer.LabelNew pDMDFull,"ScorePos3"          ,dmdscr,7,RGB(255,157,6),	0,	1,2,	75,0	,1,0
		PuPlayer.LabelNew pDMDFull,"ScorePos4"          ,dmdscr,7,RGB(255,157,6),	0,	1,2,	75,0	,1,0
		PuPlayer.LabelNew pDMDFull,"CurrentPlayerScore",dmdscr,12,RGB(255,255,255),0,1,2,0,0,1,0
		PuPlayer.LabelNew pDMDFull,"randomtxt"         ,dmdscr,12,RGB(255,255,255),0,1,1,50,82,1,0 'Static Timed Pop Text - Chris
		PuPlayer.LabelNew pDMDFull,"randomtxt2"        ,dmdscr,4,RGB(255,255,255),0,0,1,50,50,1,0 'Random Pop Text - Chris

		PuPlayer.LabelNew pDMDFull,"MsgFull"        	,dmdscr,4,RGB(255,255,255),0  ,0,0,  0,0,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg1"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,9,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg2"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,16,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg3"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,23,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg4"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,30,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg5"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,37,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg6"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,44,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg7"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,51,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg8"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,58,1,0 'Messages
		PuPlayer.LabelNew pDMDFull,"Msg9"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,67,1,0 'Messages	
		PuPlayer.LabelNew pDMDFull,"Msg10"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,72,1,0 'Messages

		PuPlayer.LabelNew pDMDFull,"lbl2M2M_1"        	,dmdscr,9,RGB(248,113,40),0  ,2,0,  47,20,1,0 'Hallowed
		PuPlayer.LabelNew pDMDFull,"txt2M2M_1"        	,dmdscr,9,RGB(248,113,40),0  ,0,0,  50,20,1,0 
		PuPlayer.LabelNew pDMDFull,"lbl2M2M_2"        	,dmdscr,9,RGB( 0,124,128),0  ,2,0,  47,28,1,0 'FEAR
		PuPlayer.LabelNew pDMDFull,"txt2M2M_2"        	,dmdscr,9,RGB(164,92,160),0  ,0,0,  50,28,1,0
		PuPlayer.LabelNew pDMDFull,"lbl2M2M_3"        	,dmdscr,9,RGB(223,220,40),0  ,2,0,  47,36,1,0 'ICARUS
		PuPlayer.LabelNew pDMDFull,"txt2M2M_3"        	,dmdscr,9,RGB(223,220,40),0  ,0,0,  50,36,1,0
		PuPlayer.LabelNew pDMDFull,"lbl2M2M_4"        	,dmdscr,9,RGB(29,203,197),0  ,2,0,  47,44,1,0 'RIME
		PuPlayer.LabelNew pDMDFull,"txt2M2M_4"        	,dmdscr,9,RGB(29,203,197),0  ,0,0,  50,44,1,0
		PuPlayer.LabelNew pDMDFull,"lbl2M2M_5"        	,dmdscr,9,RGB(41,202,215),0  ,2,0,  47,52,1,0 'ACES
		PuPlayer.LabelNew pDMDFull,"txt2M2M_5"        	,dmdscr,9,RGB(41,202,215),0  ,0,0,  50,52,1,0

		puPlayer.LabelNew pDMDFull,"ScorbitQR"			,dmdscr,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1
		puPlayer.LabelNew pDMDFull,"ScorbitQRicon"		,dmdscr,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1

	'Page 1 (Song Selection Iamges)
		PuPlayer.LabelNew pDMDFull,"SongSelectC",		dmdscr,4,RGB(255,255,255),0,0,0,7,13,1,1

	'Page 1(Power Features)
		PuPlayer.LabelNew pDMDFull,"pOverlay",			dmdscr,4,RGB(255,255,255),0,1,0,7,13,1,1
		PuPlayer.LabelNew pDMDFull,"RemainingSpinners",	dmdscr,4,RGB(255,255,255),0,1,0,7,13,1,1
	    PuPlayer.LabelNew pDMDFull,"RemainingRamps",	dmdscr,4,RGB(255,255,255),0,1,0,7,26,1,1
	    PuPlayer.LabelNew pDMDFull,"RemainingPops",		dmdscr,4,RGB(255,255,255),0,1,0,7,39,1,1
	    PuPlayer.LabelNew pDMDFull,"RemainingTargets",	dmdscr,4,RGB(255,255,255),0,1,0,7,52,1,1
		PuPlayer.LabelNew pDMDFull,"RemainingOrbits",   dmdscr,4,RGB(255,255,255),0,1,0,7,65,1,1
		PuPlayer.LabelNew pDMDFull,"pSpinners",         dmdscr,4,RGB(255,255,255),0,0,0,4,08,1,1 'Power Feature 
		PuPlayer.LabelNew pDMDFull,"pRamps",			dmdscr,4,RGB(255,255,255),0,0,0,4,21,1,1 'Power Feature 
	    PuPlayer.LabelNew pDMDFull,"pPops",				dmdscr,4,RGB(255,255,255),0,0,0,4,34,1,1 'Power Feature 
	    PuPlayer.LabelNew pDMDFull,"pTargets",			dmdscr,4,RGB(255,255,255),0,0,0,4,47,1,1 'Power Feature 
		PuPlayer.LabelNew pDMDFull,"pOrbits",  			dmdscr,4,RGB(255,255,255),0,0,0,4,60,1,1 'Power Feature 
		PuPlayer.LabelNew pDMDFull,"pJackpot",  		dmdscr,4,RGB(255,255,255),0,0,0,4,60,1,1 'Power Jackpot for Cyborg

		PuPlayer.LabelNew pDMDFull,"pSpinnersChk",         dmdscr,4,RGB(255,255,255),0,0,0,4,08,1,1 'Power Feature Complete 
		PuPlayer.LabelNew pDMDFull,"pRampsChk",			dmdscr,4,RGB(255,255,255),0,0,0,4,21,1,1 'Power Feature Complete 
	    PuPlayer.LabelNew pDMDFull,"pPopsChk",				dmdscr,4,RGB(255,255,255),0,0,0,4,34,1,1 'Power Feature Complete 
	    PuPlayer.LabelNew pDMDFull,"pTargetsChk",			dmdscr,4,RGB(255,255,255),0,0,0,4,47,1,1 'Power Feature Complete 
		PuPlayer.LabelNew pDMDFull,"pOrbitsChk",  			dmdscr,4,RGB(255,255,255),0,0,0,4,60,1,1 'Power Feature Complete 

		PuPlayer.LabelNew pDMDFull,"pREVIVE",  			dmdscr,4,RGB(255,255,255),0,0,0,1,74,1,1 'REVIVE Gifs
		PuPlayer.LabelNew pDMDFull,"pREVIVESPINS",  	dmdscr,6,RGB(255,255,255),0,1,0,6,80,1,1 'REVIVE Gifs ' 6 on x may be best

		PuPlayer.LabelNew pDMDFull,"pQuickInfo",        dmdscr,4,RGB(255,255,255),0,0,0,25,25,1,0 'Quick Info Stone
		PuPlayer.LabelSet pDMDFull,"pOverlay","PuPOverlays\\PF.png",1,"{'mt':2, 'xalign':0,'width':10.5, 'height':74, 'xpos':0.1,'ypos':1}"


		' Genral Mode and Timer Text 
		PuPlayer.LabelNew pDMDFull,"ModeTxtL1",				dmdscr,4,RGB(255,255,255),0,1,1,94,26,1,1	
		PuPlayer.LabelNew pDMDFull,"ModeTxtL2",				dmdscr,4,RGB(255,255,255),0,1,1,94,29.5,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxtL3"				,dmdscr,4,RGB(255,255,255),0,1,1,94,32,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxtL4"				,dmdscr,4,RGB(255,255,255),0,1,1,94,40,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTimer"				,dmdscr,13,RGB(255,255,255),0,1,1,94,40,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxtHL1"				,dmdscr,4.5,RGB(255,255,255),0,1,1,94,37,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxtHL2"				,dmdscr,4.5,RGB(255,255,255),0,1,1,94,41,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxt2HL1"			,dmdscr,4.5,RGB(255,255,255),0,1,1,94,10,1,1
		PuPlayer.LabelNew pDMDFull,"ModeTxt2HL2"			,dmdscr,4.5,RGB(255,255,255),0,1,1,94,13,1,1


	'Page1 (Super Slings)
		for i = 0 to 8
			puPlayer.LabelNew pDMDFull,"PopImgM" & i,				dmdscr,	4,RGB(255, 255, 255)  		,0,1,1 ,0,0  ,1,0
			puPlayer.LabelNew pDMDFull,"PopImgT" & i,				dmdscr,	4,RGB(255, 255, 255)  		,0,1,1 ,0,0  ,1,0
			puPlayer.LabelNew pDMDFull,"PopImgB" & i,				dmdscr,	4,RGB(255, 255, 255)  		,0,1,1 ,0,0  ,1,0
			puPlayer.LabelNew pDMDFull,"PopScore" & i,				dmdscr,	7,pupColorRed 		 		,0,1,1 ,0,0  ,1,0
		Next 


	'Page 1 (Mode Timers)	
		PuPlayer.LabelNew pDMDFull,"M2M_CNTBG",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT0",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT1",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT2",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT3",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT4",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		PuPlayer.LabelNew pDMDFull,"M2M_CNT5",				 dmdfixed,15,RGB(204,0,0),0  ,0,0  ,0,0,1,1
		'PuPlayer.LabelNew pDMDFull,"OrbitsCountdown",dmdscr,20,RGB(255,255,255),0,1,1,94,45,1,0
		'PuPlayer.LabelNew pDMDFull,"MadnessCountdown",dmdscr,20,RGB(255,255,255),0,1,1,94,45,1,0 ' ANDREW - could place a timer on screen --Hitting the orb locks in lit shots for 10 seconds.


    'Page 1 (Jackpot Counts)
		PuPlayer.LabelNew pDMDFull,"LoopJackPotLoopsNeeded",dmdscr,3,RGB(255,255,255),0,1,1,94,64,1,0 	'in use
		PuPlayer.LabelNew pDMDFull,"LoopJackPotLoopsCount",dmdscr,3,RGB(255,255,255),0,1,1,94,67,1,0 	'new Andrew
		PuPlayer.LabelNew pDMDFull,"pSplashLoopCount",			dmdscr,8,RGB(255,255,255),0,1,1,0,0,1,0 	'in use
		PuPlayer.LabelNew pDMDFull,"pSplashLoopCountScore",		dmdscr,8,RGB(201, 17, 0),0,1,1,0,60,1,0		'in use
		PuPlayer.LabelNew pDMDFull,"LoopJackpotLabelScore",	dmdscr,3,RGB(255,255,255),0,1,1,94,71,1,0	'in use
		PuPlayer.LabelNew pDMDFull,"LoopJackpotScore",		dmdscr,3,RGB(255,255,255),0,1,1,94,74,1,0	'in use
		PuPlayer.LabelNew pDMDFull,"Power",					dmdscr,4,RGB(255,255,255),0,0,1,87,88,1,0	'in use
		PuPlayer.LabelNew pDMDFull,"Jackpot",				dmdscr,4,RGB(255,255,255),0,0,1,87,91,1,0	'in use
		PuPlayer.LabelNew pDMDFull,"PowerJackpotMultiplier",dmdscr,6,RGB(255,255,255),0,2,1,99,90,1,0	'in use
		PuPlayer.LabelNew pDMDFull,"PowerJackpotScore",		dmdscr,4,RGB(255,255,255),0,1,1,93,96,1,0	'in use

	'Page 1 (End of Ball Bonus Totals)
		PuPlayer.LabelNew pDMDFull,"LblBonusSwitches",			dmdscr,4,RGB(255,255,255),0,2,1,41,23,1,0
	    PuPlayer.LabelNew pDMDFull,"LblBonusMummyLetters", 		dmdscr,4,RGB(255,255,255),0,2,1,41,30,1,0
	    PuPlayer.LabelNew pDMDFull,"LblBonusDeathblows", 		dmdscr,4,RGB(255,255,255),0,2,1,41,37,1,0
	    PuPlayer.LabelNew pDMDFull,"LblBonusLoops",				dmdscr,4,RGB(255,255,255),0,2,1,41,44,1,0
		PuPlayer.LabelNew pDMDFull,"LblBonusPowerFeatures",		dmdscr,4,RGB(255,255,255),0,2,1,41,51,1,0  ' needs to be counted
		PuPlayer.LabelNew pDMDFull,"LblBonusSoulShards",		dmdscr,4,RGB(255,255,255),0,2,1,41,58,1,0  ' needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard
		PuPlayer.LabelNew pDMDFull,"LblBonusEDDIESCollected",	dmdscr,4,RGB(255,255,255),0,2,1,41,65,1,0   

		PuPlayer.LabelNew pDMDFull,"CntBonusSwitches",			dmdscr,4,RGB(255,255,255),0,2,1,51,23,1,0
	    PuPlayer.LabelNew pDMDFull,"CntBonusMummyLetters", 		dmdscr,4,RGB(255,255,255),0,2,1,51,30,1,0
	    PuPlayer.LabelNew pDMDFull,"CntBonusDeathblows", 		dmdscr,4,RGB(255,255,255),0,2,1,51,37,1,0
	    PuPlayer.LabelNew pDMDFull,"CntBonusLoops",				dmdscr,4,RGB(255,255,255),0,2,1,51,44,1,0
		PuPlayer.LabelNew pDMDFull,"CntBonusPowerFeatures",		dmdscr,4,RGB(255,255,255),0,2,1,51,51,1,0  ' needs to be counted
		PuPlayer.LabelNew pDMDFull,"CntBonusSoulShards",		dmdscr,4,RGB(255,255,255),0,2,1,51,58,1,0  ' needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard
		PuPlayer.LabelNew pDMDFull,"CntBonusEDDIESCollected",	dmdscr,4,RGB(255,255,255),0,2,1,51,65,1,0   

		PuPlayer.LabelNew pDMDFull,"BonusSwitches",				dmdscr,4,RGB(255,255,255),0,2,1,64,23,1,0
	    PuPlayer.LabelNew pDMDFull,"BonusMummyLetters", 		dmdscr,4,RGB(255,255,255),0,2,1,64,30,1,0
	    PuPlayer.LabelNew pDMDFull,"BonusDeathblows", 			dmdscr,4,RGB(255,255,255),0,2,1,64,37,1,0
	    PuPlayer.LabelNew pDMDFull,"BonusLoops",				dmdscr,4,RGB(255,255,255),0,2,1,64,44,1,0
		PuPlayer.LabelNew pDMDFull,"BonusPowerFeatures",		dmdscr,4,RGB(255,255,255),0,2,1,64,51,1,0  ' needs to be counted
		PuPlayer.LabelNew pDMDFull,"BonusSoulShards",			dmdscr,4,RGB(255,255,255),0,2,1,64,58,1,0  ' needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard
		PuPlayer.LabelNew pDMDFull,"BonusEDDIESCollected",		dmdscr,4,RGB(255,255,255),0,2,1,64,65,1,0   
		PuPlayer.LabelNew pDMDFull,"BonusEddieLetters",		 	dmdscr,4,RGB(255,255,255),0,0,1,50,72,1,0   
'		PuPlayer.LabelNew pDMDFull,"BonusSpinners",				dmdscr,4,RGB(255,255,255),0,0,1,50,79,1,0
'		PuPlayer.LabelNew pDMDFull,"BonusRamps",				dmdscr,4,RGB(255,255,255),0,0,1,50,86,1,0
'		PuPlayer.LabelNew pDMDFull,"BonusPops",					dmdscr,4,RGB(255,255,255),0,0,1,50,93,1,0
'		PuPlayer.LabelNew pDMDFull,"BonusTargets",				dmdscr,4,RGB(255,255,255),0,0,1,50,96,1,0
		PuPlayer.LabelNew pDMDFull,"BonusTwoMinutesToMidnight", dmdscr,4,RGB(255,255,255),0,0,1,50,96,1,0
		PuPlayer.LabelNew pDMDFull,"BonusTotal",				dmdscr,12,RGB(255,255,255),0,1,2,0,80,1,0
		PuPlayer.LabelNew pDMDFull,"BeastShardPoints",			dmdscr,6,RGB(0,0,0),0,0,1,55,30,1,0
	    PuPlayer.LabelNew pDMDFull,"AcesShardPoints", 			dmdscr,6,RGB(0,0,0),0,0,1,55,38,1,0
	    PuPlayer.LabelNew pDMDFull,"FearShardPoints", 			dmdscr,6,RGB(0,0,0),0,0,1,55,46,1,0
	    PuPlayer.LabelNew pDMDFull,"MarinerShardPoints",		dmdscr,6,RGB(0,0,0),0,0,1,55,54,1,0
		PuPlayer.LabelNew pDMDFull,"HallowedShardPoints",		dmdscr,6,RGB(0,0,0),0,0,1,55,62,1,0  ' needs to be counted
		PuPlayer.LabelNew pDMDFull,"IcarusShardPoints",			dmdscr,6,RGB(0,0,0),0,0,1,55,70,1,0  ' needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard
		PuPlayer.LabelNew pDMDFull,"NOTBShardTotal",			dmdscr,12,RGB(0,0,0),0,1,2,0,87,1,0
		PuPlayer.LabelNew pDMDFull,"NOTBBeastHealth",			dmdscr,11,RGB(255,255,255),0,1,2,67,75,1,0

	'Page 1 (End of MODE Totals)
		PuPlayer.LabelNew pDMDFull,"HallowedTotal"	,dmdscr,9,RGB(201, 17, 0),0,1,0,50,70,1,0 ' RED
		PuPlayer.LabelNew pDMDFull,"IcarusTotal"	,dmdscr,9,RGB(201, 17, 0),0,1,0,50,70,1,0 ' RED

	'Page 1 (Instant INFO)
		puPlayer.LabelNew pDMDFull,"InstantInfoBG",dmdscr,		10,RGB(255, 255, 255)			,0,0,0 ,0,0  ,1,0
		puPlayer.LabelSet pDMDFull,"InstantInfoBG", "PuPOverlays\\Info1.png" ,0,"{'mt':2,'color':111111, 'anigif':100, 'width':100, 'height':100,'yalign':0,'xalign':0,'ypos':0,'xpos':0}"
		puPlayer.LabelNew pDMDFull,"InstantInfoL1",dmdscr,		10,RGB(255,255,255)			,0,1,0 ,0,35    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoL2",dmdscr,		10,RGB(255,255,255)			,0,1,0 ,0,45    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoL3",dmdscr,		10,RGB(255,255,255)			,0,1,0 ,0,55    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoImg1",dmdscr,		10,RGB(255,255,255)		,0,1,0 ,0,35    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoImg2",dmdscr,		10,RGB(255,255,255)		,0,1,0 ,0,35    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoImg3",dmdscr,		10,RGB(255,255,255)		,0,1,0 ,0,35    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoImg4",dmdscr,		10,RGB(255,255,255)		,0,1,0 ,0,35    ,1,1
		puPlayer.LabelNew pDMDFull,"InstantInfoImg5",dmdscr,		10,RGB(255,255,255)		,0,1,0 ,0,35    ,1,1

		puPlayer.LabelSet pDMDFull,"InstantInfoImg1", "PuPOverlays\\clear1.png" ,1,"{'mt':2, 'xpos':33,'ypos':24.5, 'width':4, 'height':11}"
		puPlayer.LabelSet pDMDFull,"InstantInfoImg2", "PuPOverlays\\clear1.png" ,1,"{'mt':2, 'xpos':33,'ypos':34, 'width':4, 'height':11}"
		puPlayer.LabelSet pDMDFull,"InstantInfoImg3", "PuPOverlays\\clear1.png" ,1,"{'mt':2, 'xpos':33,'ypos':43.5, 'width':4, 'height':11}"
		puPlayer.LabelSet pDMDFull,"InstantInfoImg4", "PuPOverlays\\clear1.png" ,1,"{'mt':2, 'xpos':33,'ypos':53, 'width':4, 'height':11}"
		puPlayer.LabelSet pDMDFull,"InstantInfoImg5", "PuPOverlays\\clear1.png" ,1,"{'mt':2, 'xpos':33,'ypos':62.5, 'width':4, 'height':11}"

	'Page 2 (Blank Page)
		PuPlayer.LabelNew pDMDFull,"Dummy1"		,dmdscr,12,RGB(255,255,255),0,1,2,0,0,2,1
		
	'Page 3 (Show High Scores Fixed Fonts)
		puPlayer.LabelNew pDMDFull,"EnterHS1",dmdscr,			8, pupColorBlack  		,0,1,1 ,0,5   	,3,1				' HS Entry
		puPlayer.LabelNew pDMDFull,"EnterHS2",dmdscr,			16,pupColorRed  		,0,1,1 ,60,35   ,3,1				' HS Entry
		for i = 1 to 11
			puPlayer.LabelNew pDMDFull,"EnterHS3" & i,  monoFont,18,	pupColorBlack  	,0,0,1 ,25+(i*5),60   ,3,1				' HS Entry
			puPlayer.LabelNew pDMDFull,"EnterHSU3" & i, monoFont, 18,	pupColorRed  	,0,0,1 ,25+(i*5),60.1 ,3,1				' HS Entry
			puPlayer.LabelSet pDMDFull,"EnterHSU3" & i, " ", 1, ""
			if i <= 3 then puPlayer.LabelNew pDMDFull,"EnterHS3Glyph" & i,  glyphsFont,18,RGB(64, 58, 46)  ,0,1,1 ,12, 15+(i*18)   ,3,1				' HS Entry
		Next 
		puPlayer.LabelNew pDMDFull,"EnterHS4"   ,monoFont,		9,pupColorBlack  	,0,0,1 ,0,95   ,3,1				' HS Entry
		puPlayer.LabelNew pDMDFull,"EnterHS4Sel",monoFont,		10,pupColorRed  	,0,1,1 ,0,95   ,3,1				' HS Entry
		puPlayer.LabelSet pDMDFull,"EnterHS4Sel", "     ", 1, ""

		PuPlayer.LabelNew pDMDFull,"HSIntro1"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,37,3,0 'Messages
		PuPlayer.LabelNew pDMDFull,"HSIntro2"        		,dmdscr,4,RGB(255,255,255),0  ,1,0,  0,58,3,0 'Messages


'		PuPlayer.LabelNew pDMDFull,"FinalScore"		,dmdscr,12,RGB(255,255,255),0,1,2,0,0,3,1
'		puPlayer.LabelNew pDMDFull,"Congrats"		,dmdalt,11,RGB(4,1,0),0,1,0,0,12,3,1	' HS Entry
'		puPlayer.LabelNew pDMDFull,"EnterHSPlayer"	,dmdalt,11,RGB(4,1,0),0,1,0,0,23,3,1	' HS Entry
'		puPlayer.LabelNew pDMDFull,"EnterHSInitials",dmdalt,11,RGB(4,1,0),0,1,0,0,34,3,1	' HS Entry
'		puPlayer.LabelNew pDMDFull,"EnterHS3"		,dmdalt,20,RGB(4,1,0),0,1,0,60,52,3,1	' HS Entry
	
		PuPlayer.LabelShowPage pDMDFull,2,0,""

		puPlayer.LabelSet pDMdFull, "P1Bg", "PupOverlays\\clear1.png",1,"{'mt':2,'zback':1}"
		puPlayer.LabelSet pDMdFull, "P2Bg", "PupOverlays\\clear1.png",1,"{'mt':2,'zback':1}"
		puPlayer.LabelSet pDMdFull, "P3Bg", "PupOverlays\\clear1.png",1,"{'mt':2,'zback':1}"
		puPlayer.LabelSet pDMdFull, "P4Bg", "PupOverlays\\clear1.png",1,"{'mt':2,'zback':1}"
		puPlayer.LabelSet pDMdFull, "HUD1", "PupOverlays\\clear1.png",1,"{'mt':2,'width':16, 'height':31, 'xpos':100 ,'ypos':100, 'xalign':2, 'zback':1}"
		puPlayer.LabelSet pDMdFull, "HUD2", "PupOverlays\\clear1.png",1,"{'mt':2,'width':16, 'height':45, 'xpos':100 ,'ypos':90, 'xalign':2, 'zback':1}"
		puPlayer.LabelSet pDMdFull, "HUD3", "PupOverlays\\clear1.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':52, 'xalign':2, 'zback':1}"
		puPlayer.LabelSet pDMdFull, "HUD4", "PupOverlays\\clear1.png",1,"{'mt':2,'width':16, 'height':35, 'xpos':100 ,'ypos':52, 'xalign':2, 'zback':1}"


	' Transparent 
	' Page 1 (Power features glow)
		PuPlayer.LabelNew pTransp,"pSpinnersG",         dmdscr,4,RGB(255,255,255),0,0,0,4,08,1,1 'Power FeatureGlow 
		PuPlayer.LabelNew pTransp,"pRampsG",			dmdscr,4,RGB(255,255,255),0,0,0,4,21,1,1 'Power FeatureGlow 
	    PuPlayer.LabelNew pTransp,"pPopsG",				dmdscr,4,RGB(255,255,255),0,0,0,4,34,1,1 'Power FeatureGlow 
	    PuPlayer.LabelNew pTransp,"pTargetsG",			dmdscr,4,RGB(255,255,255),0,0,0,4,47,1,1 'Power FeatureGlow 
		PuPlayer.LabelNew pTransp,"pOrbitsG",  			dmdscr,4,RGB(255,255,255),0,0,0,4,60,1,1 'Power FeatureGlow 

	' Page 1 (Song selection)
		PuPlayer.LabelNew pTransp,"SongSelectLT",		dmdscr,4,RGB(255,255,255),0,0,0,7,13,1,1
		PuPlayer.LabelNew pTransp,"SongSelectCT",		dmdscr,4,RGB(255,255,255),0,0,0,7,13,1,1
	    PuPlayer.LabelNew pTransp,"SongSelectRT",		dmdscr,4,RGB(255,255,255),0,0,0,7,26,1,1

	' Page2 (Clear)
		PuPlayer.LabelNew pTransp,"Dummy1"		,dmdscr,12,RGB(255,255,255),0,1,2,0,0,2,1

		PuPlayer.LabelShowPage pTransp,1,0,""
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pTransp &", ""FN"":22, ""AM"":1, ""AV"":80 }"		' Make Transparent 

		
		puPlayer.LabelInit pOverVid
		PuPlayer.LabelNew pOverVid,"Dummy1"		,dmdscr,12,RGB(255,255,255),0,1,2,0,0,1,1

		' Page 3 (Service)
		puPlayer.LabelNew pOverVid,"ServiceL1",serviceFont,		8,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		puPlayer.LabelNew pOverVid,"ServiceL2",serviceFont,		8,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		puPlayer.LabelNew pOverVid,"ServiceL3",serviceFont,   	8,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		puPlayer.LabelNew pOverVid,"ServiceL3B",serviceFont2,  20,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		puPlayer.LabelNew pOverVid,"ServiceL3Img",serviceFont2, 1,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		puPlayer.LabelNew pOverVid,"ServiceL4",serviceFont,		8,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		For i = 0 to 50
			puPlayer.LabelNew pOverVid,"ServiceV"&i,serviceFont,	8,RGB(252, 239, 3)  ,0,1,0 ,0,1    ,3,1
		Next 

	 ' Page 4 (Attract)
		puPlayer.LabelNew pDMDFull,"SplashImg",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashImg1",dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashImg2",dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"Splash1",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"Splash2",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"Splash3",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"Splash4",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashHS1",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashHS2",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashHS3",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"SplashHS4",	dmdscr,		10,RGB(255,255,255)				,0,1,1 ,0,0    ,4,1

		puPlayer.LabelNew pDMDFull,"ScorbitQR_a"		,dmdscr,	1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"ScorbitQRicon_a"	,dmdscr,	1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,4,1


		' Scrolling HS Stats
		puPlayer.LabelNew pDMDFull,"StatsScore1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsName1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1L1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1L2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1L3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1R1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1R2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit1R3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1

		puPlayer.LabelNew pDMDFull,"StatsScore2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsName2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2L1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2L2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2L3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2R1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2R2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit2R3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1

		puPlayer.LabelNew pDMDFull,"StatsScore3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsName3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3L1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3L2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3L3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3R1",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3R2",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1
		puPlayer.LabelNew pDMDFull,"StatsInit3R3",	dmdscr,		10,RGB(255,255,255)				,0,1,2 ,0,0    ,4,1

		PuPlayer.LabelShowPage pOverVid, 1, 0,""

	END IF  ' use PuPDMDDriver
end Sub 'page Layouts


'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
Sub pClearEverything ' ANDREW - This Clears Down Almost Everything except SCORE
WriteToLog "     ", "pClearEverything"
	pStopTimersAndModeTxt
	pClearRandomTxt
	pClearPowerFeatures
	pClearJackpotCounts
	pClearBonusTxt
	puPlayer.LabelSet pDMDFull,"pREVIVE","PuPOverlays\\NO OVERLAY.png",1,""
	puPlayer.LabelSet pDMDFull,"pREVIVESpins"," ",1,""
End Sub

Sub pClearEverything2 ' DAP - This Clears Almost Everything except SCORE, jackpots, and loops
WriteToLog "     ", "pClearEverything2"
	pStopTimersAndModeTxt
	pClearRandomTxt
	pClearPowerFeatures
'	pClearJackpotCounts
	pClearBonusTxt
	puPlayer.LabelSet pDMDFull,"pREVIVE","PuPOverlays\\NO OVERLAY.png",1,""
	puPlayer.LabelSet pDMDFull,"pREVIVESpins"," ",1,""
End Sub

Sub pStopTimersAndModeTxt ' ANDREW - This Stops ALL Timers and Mode Txt - all this stuff can and needs to be summarized as its getting out of control (came from clearrandomtxt) lol
'	NOTBBeastHealthOff
	tmrNOTB.Enabled = 0
	TwoMinToMidnightTimer.Enabled = 0
	FearOfTheDarkTimer.Enabled = 0
	MarinerTimer1.Enabled = 0
	MarinerTimer2.Enabled = 0
	IcarusTimer.Enabled = 0
	HallowedTimer.Enabled = 0

	PuPlayer.LabelSet pDMDFull,"ModeTxtL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL2"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL3"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL4"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTimer"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtHL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtHL2"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxt2HL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxt2HL2"," ",1,"" 

	PuPlayer.LabelSet pDMDFull,"HallowedTotal"," ",1,"" 
End Sub

Sub pClearShardPoints 'ANDREW
	PuPlayer.LabelSet pDMDFull,"BeastShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"AcesShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"FearShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"MarinerShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"HallowedShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"IcarusShardPoints","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"BonusTotal","",1,"  " 
End Sub

Sub pClearJackpotCounts 'ANDREW
WriteToLog "     ", "pClearJackpotCounts:"
	Puplayer.LabelSet pDMDFull,"Power"," ",1,""  
	Puplayer.LabelSet pDMDFull,"Jackpot"," ",1,""  
	Puplayer.LabelSet pDMDFull,"PowerJackpotMultiplier"," ",1,""  
	PuPlayer.LabelSet pDMDFull,"PowerJackPotScore"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsNeeded"," ",1,""
	PuPlayer.LabelSet pDMDFull,"LoopJackPotLoopsCount"," ",1,""
	PuPlayer.LabelSet pDMDFull,"LoopJackpotLabelScore"," ",1, "" 
	PuPlayer.LabelSet pDMDFull,"LoopJackpotScore"," ",1, "" 
end Sub

Sub pClearSplashLoopCount 'ANDREW
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCount","" ,1,"" '
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCountScore","",1,"" 
End Sub

Sub pClearBonusTxt 'ANDREW
	PuPlayer.LabelSet pDMDFull,"LblBonusSwitches","",1,"  "
	PuPlayer.LabelSet pDMDFull,"LblBonusMummyLetters","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"LblBonusDeathblows","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"LblBonusLoops","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"LblBonusPowerFeatures","",1,"" ' Andrew  ' needs to be counted 
	PuPlayer.LabelSet pDMDFull,"LblBonusSoulShards","",1,"" ' Andrew needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard etc...
	PuPlayer.LabelSet pDMDFull,"LblBonusEDDIESCollected","",1,"  "
	PuPlayer.LabelSet pDMDFull,"LblBonusTotal"," ",1," "

	PuPlayer.LabelSet pDMDFull,"CntBonusSwitches","",1,"  "
	PuPlayer.LabelSet pDMDFull,"CntBonusMummyLetters","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"CntBonusDeathblows","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"CntBonusLoops","",1,"  " 
	PuPlayer.LabelSet pDMDFull,"CntBonusPowerFeatures","",1,"" ' Andrew  ' needs to be counted 
	PuPlayer.LabelSet pDMDFull,"CntBonusSoulShards","",1,"" ' Andrew needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard etc...
	PuPlayer.LabelSet pDMDFull,"CntBonusEDDIESCollected","",1,"  "
	PuPlayer.LabelSet pDMDFull,"CntBonusTotal"," ",1," "

	PuPlayer.LabelSet pDMDFull,"BonusSwitches"," ",1,"  "
	PuPlayer.LabelSet pDMDFull,"BonusMummyLetters"," ",1,"  " 
	PuPlayer.LabelSet pDMDFull,"BonusDeathblows"," ",1,"  " 
	PuPlayer.LabelSet pDMDFull,"BonusLoops"," ",1,"  " 
	PuPlayer.LabelSet pDMDFull,"BonusPowerFeatures"," ",1,"" ' Andrew  ' needs to be counted 
	PuPlayer.LabelSet pDMDFull,"BonusSoulShards"," ",1,"" ' Andrew needs to be implemented then counted (at end of hallowed you need to add a second bullseye event to collect the shard etc...
	PuPlayer.LabelSet pDMDFull,"BonusEDDIESCollected"," ",1,"  "
	PuPlayer.LabelSet pDMDFull,"BonusTotal"," ",1," "	
End Sub

'ANDREW Reorganized this to make it more manageable.

'This sub is called to clear the random txt throughout the table - Chris
Sub pClearRandomTxt()
	PuPlayer.LabelSet pDMDFull,"randomtxt","",1,"" 
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCount","",1,"" 
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCountScore","",1,""
	PuPlayer.LabelSet pDMDFull,"HallowedTotal","",1,"" 

	PuPlayer.LabelSet pDMDFull,"ModeTxtL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL2"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL3"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtL4"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTimer"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtHL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxtHL2"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxt2HL1"," ",1,"" 
	PuPlayer.LabelSet pDMDFull,"ModeTxt2HL2"," ",1,"" 

End Sub 

Sub pClearRandomTxt2()
	PuPlayer.LabelSet pDMDFull,"randomtxt2","",1,"" 
End Sub 

'Added clear random loop txt seperately from pClearRandomTxt, by having it in the loop section "Spell Eddie" was from clearing the screen randomly
Sub pClearLoopTxt()
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCount","",1,"" 
	PuPlayer.LabelSet pDMDFull,"pSplashLoopCountScore","",1,""
End Sub


Sub pDMDStartGame
	TimerStats(False)
	pInAttract=false

	PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\clear.png",0,""
	PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\clear.png",0,""

	ClearAttract
	pDMDSetPage(pScores)   'set blank text overlay page.
end Sub


Sub pDMDStartBall
end Sub

'Sub pDMDGameOver
'	pAttractStart
'
''	PuPEvent 500
''	PuPEventD 0
'end Sub
'
Sub pAttractStart(bReset)
WriteToLog "     ", "pAttractStart"
	PuPlayer.LabelShowPage pDMDFull, 4,0,""
	PriorityReset=1
	if (pInAttract = False) then 
		WriteToLog "     ", "STARTING ATTRACT"
		if bReset then pCurAttractPos=0
WriteToLog "     ", "ATTRACT Next:" & pCurAttractPos
		pAttractReset=-1
		pDMDCurPriority=-1
		PriorityReset=1
		pInAttract=true
	end if 
end Sub


'Sub pDMDStartUP
'	'WriteToLog "     ", "STARTING ATTRACT2"
'	pupDMDDisplay "501","WELL...WE'RE WAITING!","welcome.mp4",4,0,1
'	'pupDMDDisplay "attract","Game Over","vidIntro.mp4",9,0,10
''	PuPlayer.LabelShowPage pOverVid, 1,0,""
''	PuPlayer.playlistplayex pOverVid,"PupOverlays","Attract1.png", 1, 1
''	PuPlayer.LabelSet pOverVid, "Splash1", "GAME OVER", 1, "{'mt':2,'size':20,'ypos':50}"
''
''	PuPlayer.playlistplayex pMVideo,"PupVideos","black800x600.mp4", 1, 1
''	PuPlayer.SetLoop pMVideo, 1
'
'	pAttractReset=-1
'	pDMDCurPriority=-1
'	PriorityReset=1
'	pInAttract=true
'end Sub 

Sub ClearAttract()  ' Clear all Attract field 
	PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
	PuPlayer.LabelSet pDMDFull, "Splash1", ""	, 1, "{'mt':2,'size':20,'xpos':0,'ypos':50}"
	PuPlayer.LabelSet pDMDFull, "Splash2", ""	, 1, ""
	PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""
	PuPlayer.LabelSet pDMDFull, "Splash4", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'xpos':0, 'ypos':70}"
	PuPlayer.LabelSet pDMDFull, "SplashHS1", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'xpos':0, 'ypos':50}"
	PuPlayer.LabelSet pDMDFull, "SplashHS2", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'xpos':0, 'ypos':50}"
	PuPlayer.LabelSet pDMDFull, "SplashHS3", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'xpos':0, 'ypos':70}"
	PuPlayer.LabelSet pDMDFull, "SplashHS4", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'xpos':0, 'ypos':70}"
End Sub 


Dim AttractRndVideoIdx:AttractRndVideoIdx=1
DIM pCurAttractPos: pCurAttractPos=0
Const pAttractLastScore   = 2
Const pAttractScrollStart = 4
Const pAttractScrollEnd   = 17
Const pAttractEndOfLoop   = 19
Const pAttractCredit      = 20

'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext()
Dim TmpStr1
Dim TmpStr2
Dim i
Dim cnt
dim yPos

pCurAttractPos=pCurAttractPos+1
WriteToLog "     ", "ATTRACT Next:" & pCurAttractPos & " LFPress:" & LFPress & " RFPress:" & LFPress

PriorityReset=4000

  Select Case pCurAttractPos

	case 1:
		playclear pDMDFull
		pAttractReset=-1:PriorityReset=3000
		ClearAttract
		if AttractRndVideoIdx<>0 then AttractRndVideoIdx=INT(RND*3)+1
		select case AttractRndVideoIdx
			case 0:
'				PlayMusic "MusicEndOfGame.mp3"		' Dont loop
				playclear pMusic
				playmedia "MusicEndOfGame.mp3", MusicDir, pMusic, "", 250486, "", 1, 1
				PuPlayer.playlistplayex pDMDFull,"Callouts","EndOfGame.mp4", cVolBGVideo*100, 1
			case 1 :
				PuPlayer.playlistplayex pDMDFull,"MummyMultiball","Scepter.mp4", cVolBGVideo*0, 1
			case 2:
				PuPlayer.playlistplayex pDMDFull,"FearOfTheDark","FearOfDarkLoop.mp4", cVolBGVideo*0, 1
			case 3:
				PuPlayer.playlistplayex pDMDFull,"FlightOfIcarus","Flight BG Loop.mp4", cVolBGVideo*0, 1
		End Select 
		PuPlayer.SetLoop pDMDFull, 1

		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
		PuPlayer.LabelSet pDMDFull, "Splash1", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "Splash2", "GAME OVER"	, 1, "{'mt':2,'ypos':45, 'size':9, 'color':" & pupColorRed & "}"
		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""

	case 2:											' pAttractLastScore
		pAttractReset=-1:PriorityReset=3000

'		LastScore(0)=1300300000
'		LastScore(1)=1300300000
'		LastScore(2)=1300300000
'		LastScore(3)=1300300000

		if LastScore(0)<>0 then 
			TimerStats(False) 
			ClearAttract

			PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
			PuPlayer.LabelSet pDMDFull, "Splash1", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'ypos':50}"
			PuPlayer.LabelSet pDMDFull, "Splash2", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'ypos':50}"
			PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'ypos':70}"
			cnt=0
			for i = 0 to MaxPlayers-1
				if LastScore(i)<> 0 then cnt=cnt+1
			Next

			for i = 0 to MaxPlayers-1
				If LastScore(i)<>0 then
					yPos=(i+1)*(100\(cnt+1))-10
					PuPlayer.LabelSet pDMDFull, "Splash" & i+1  ,"Player " & i+1 & " ", 1,           "{'mt':2,'size':" & 9 - cnt & ",'xalign':1,'yalign':1,'ypos':" & yPos & ",'xpos':" & (i+1)*(50\(cnt+1)) + 25 & ",'color':" & pupColorGrey & "}"
					PuPlayer.LabelSet pDMDFull, "SplashHS"& i+1 ,FormatScore(LastScore(i)), 1, "{'mt':2,'size':" & 13 - cnt & ",'xalign':1,'yalign':1,'ypos':" & yPos +(13-cnt) & ",'xpos':" & (i+1)*(50\(cnt+1)) + 25 & ",'color':" & pupColorGrey & "}"
					' TBD Add trophy icons 
				End if 
			Next 
		else 
			pAttractNext 'No last high score 
		End if

	case 3:
		pAttractReset=-1:PriorityReset=3000
		ClearAttract

		if (Scorbit.bNeedsPairing) then 
			PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
			PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
		End if 

		TimerStats(False) 
		playclear pDMDFull
		select case AttractRndVideoIdx
			case 0:
				PuPlayer.playlistplayex pDMDFull,"Callouts","EndOfGame.mp4", cVolBGVideo*0, 1
			case 1 :
				PuPlayer.playlistplayex pDMDFull,"MummyMultiball","Scepter.mp4", cVolBGVideo*0, 1
			case 2:
				PuPlayer.playlistplayex pDMDFull,"FearOfTheDark","FearOfDarkLoop.mp4", cVolBGVideo*0, 1
			case 3:
				PuPlayer.playlistplayex pDMDFull,"FlightOfIcarus","Flight BG Loop.mp4", cVolBGVideo*0, 1
		End Select 
		AttractRndVideoIdx=1

		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
		PuPlayer.LabelSet pDMDFull, "Splash1", "REPLAY AT", 			1, "{'mt':2,'ypos':40, 'size':9, 'color':" & pupColorGrey & "}"
		PuPlayer.LabelSet pDMDFull, "Splash2", FormatScore(DMDStd(kDMDStd_DynReplayStart)), 	1, "{'mt':2,'ypos':50, 'size':9, 'color':" & pupColorGrey & "}"
		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""


	case 4:						' pAttractScrollStart
		playclear pDMDFull
		ClearAttract
		TimerStats(True) 
	case 5:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 6:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 7:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 8:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 9:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 10:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 11:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 12:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 13:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 14:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 15:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 16:
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 17:						' pAttractScrollEnd
		tmrStats.Interval=1
		tmrStats.Enabled=True 
	case 18:
		TimerStats(False) 
		playclear pDMDFull
		pAttractReset=-1:PriorityReset=574720
		PuPlayer.playlistplayex pDMDFull,"Backglass","Tour.mp4", cVolBGVideo*100, 1
		PuPlayer.SetLoop pDMDFull, 1
		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
		PuPlayer.LabelSet pDMDFull, "Splash1", "",			      1, "{'mt':2, 'size':9, 'ypos':78, 'color':" & pupColorGrey & "}"
		PuPlayer.LabelSet pDMDFull, "Splash2", "", 1, "{'mt':2, 'size':9, 'ypos':90, 'color':" & RGB(132,89,39) & "}"
		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""

	case 19:						' pAttractEndOfLoop
		pCurAttractPos=21
		pAttractReset=-1:PriorityReset=3000

		playclear pDMDFull
		select case AttractRndVideoIdx
			case 1 :
				PuPlayer.playlistplayex pDMDFull,"MummyMultiball","Scepter.mp4", cVolBGVideo*0, 1
			case 2:
				PuPlayer.playlistplayex pDMDFull,"FearOfTheDark","FearOfDarkLoop.mp4", cVolBGVideo*0, 1
			case 3:
				PuPlayer.playlistplayex pDMDFull,"FlightOfIcarus","Flight BG Loop.mp4", cVolBGVideo*0, 1
		End Select 
		PuPlayer.SetLoop pDMDFull, 1
		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\Songs.png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':0}"
		PuPlayer.LabelSet pDMDFull, "Splash1", "",			      1, "{'mt':2, 'size':9, 'ypos':78, 'color':" & pupColorGrey & "}"
		PuPlayer.LabelSet pDMDFull, "Splash2", "", 1, "{'mt':2, 'size':9, 'ypos':90, 'color':" & RGB(132,89,39) & "}"
		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""


	case 20:			' pAttractCredit
		playclear pDMDFull
		pAttractReset=-1:PriorityReset=2000
		PuPlayer.playlistplayex pDMDFull,"Eddie","Eddie.mp4", cVolBGVideo*100, 1
		PuPlayer.SetLoop pDMDFull, 1		

		ClearAttract

		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\Logo.png", 1, "{'mt':2,'width':75,'height':40, 'xalign':1, 'yalign':0}"
		PuPlayer.LabelSet pDMDFull, "Splash1", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "Splash2", "CREDITS"	, 1, "{'mt':2,'ypos':50, 'size':9, 'color':" & pupColorGrey & "}"
		if Credits=0 then 
			PuPlayer.LabelSet pDMDFull, "Splash3", Credits		, 1, "{'mt':2,'ypos':63, 'size':9, 'color':" & pupColorRed & "}"
		Else 
			PuPlayer.LabelSet pDMDFull, "Splash3", Credits		, 1, "{'mt':2,'ypos':63, 'size':9, 'color':" & pupColorGreen & "}"
		End if 

'' CALLED AT GAME OVER 
'	case 20:
'		
'		ClearAttract
'
'		playclear pDMDFull
'		PriorityReset=2000
'		PlayMusic "MusicEndOfGame.mp3"
'		PuPlayer.playlistplayex pDMDFull,"Callouts","EndOfGame.mp4", cVolBGVideo*100, 1
'		PuPlayer.SetLoop pDMDFull, 1
'
'		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
'		PuPlayer.LabelSet pDMDFull, "Splash1", "", 1, ""
'		PuPlayer.LabelSet pDMDFull, "Splash2", "GAME OVER"	, 1, "{'mt':2,'ypos':45, 'size':9, 'color':" & pupColorRed & "}"
'		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""
'	case 21:
'		PriorityReset=3000
'
'		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
'		PuPlayer.LabelSet pDMDFull, "Splash1", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'ypos':50}"
'		PuPlayer.LabelSet pDMDFull, "Splash2", ""	, 1, "{'mt':2,'size':14, 'xalign':1, 'ypos':50}"
'		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'ypos':70}"
'		PuPlayer.LabelSet pDMDFull, "Splash4", ""	, 1, "{'mt':2,'size':10, 'xalign':1, 'ypos':70}"
'		cnt=0
'		for i = 0 to MaxPlayers-1
'			if LastScore(i)<> 0 then cnt=cnt+1
'		Next
'
'		for i = 0 to MaxPlayers-1
'			If LastScore(i)<>0 then
'				yPos=(i+1)*(100\(cnt+1))-10
'				PuPlayer.LabelSet pDMDFull, "Splash" & i+1  ,"Player " & i+1 & " ", 1,           "{'mt':2,'size':" & 9 - cnt & ",'xalign':1,'yalign':1,'ypos':" & yPos & ",'xpos':" & (i+1)*(50\(cnt+1)) + 25 & ",'color':" & pupColorGrey & "}"
'				PuPlayer.LabelSet pDMDFull, "SplashHS"& i+1 ,FormatScore(LastScore(i)), 1, "{'mt':2,'size':" & 13 - cnt & ",'xalign':1,'yalign':1,'ypos':" & yPos +(13-cnt) & ",'xpos':" & (i+1)*(50\(cnt+1)) + 25 & ",'color':" & pupColorGrey & "}"
'				' TBD Add trophy icons 
'			End if 
'		Next 
'
'	case 22:
'		PriorityReset=3000
'		ClearAttract
'
'		PuPlayer.LabelSet pDMDFull, "SplashImg", "PupOverlays\\clear.png", 1, ""
'		PuPlayer.LabelSet pDMDFull, "Splash1", "REPLAY AT", 			1, "{'mt':2,'ypos':40, 'size':9, 'color':" & pupColorGrey & "}"
'		PuPlayer.LabelSet pDMDFull, "Splash2", FormatScore(DMDStd(kDMDStd_DynReplayStart)), 	1, "{'mt':2,'ypos':50, 'size':9, 'color':" & pupColorGrey & "}"
'		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""
'

	Case Else
		pCurAttractPos=0
		pAttractNext 'reset to beginning
	end Select

end Sub

Dim TimerStats_Enabled:TimerStats_Enabled=False 
Sub TimerStats(bEnable)	' Scrolling Stats Screen
	TimerStats_Enabled=bEnable
	If bEnable Then
		PuPlayer.playlistplayex pDMDFull,"Backglass","Cloud.mp4", cVolBGVideo*100, 1
		PuPlayer.SetLoop pDMDFull, 1
		PuPlayer.LabelSet pDMDFull, "SplashImg1", "Backglass\\Stone1.png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':2, 'ypos':100, 'zback':1}"
		PuPlayer.LabelSet pDMDFull, "SplashImg2", "Backglass\\Stone2.png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':0, 'ypos':100, 'zback':1}"
		PuPlayer.LabelSet pDMDFull, "Splash1", HighScoreName(0),	      1, "{'mt':2, 'size':9, 'ypos':78, 'color':" & pupColorBrown & "}"
		PuPlayer.LabelSet pDMDFull, "Splash2", FormatScore(HighScore(0)), 1, "{'mt':2, 'size':9, 'ypos':90, 'color':" & pupColorBrown & "}"
		PuPlayer.LabelSet pDMDFull, "Splash3", ""	, 1, ""
		PuPlayer.LabelSet pDMDFull, "StatsName1", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore1", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "StatsName2", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore2", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "StatsName3", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore3", "", 1, ""

		tmrStats2_idx=0
		tmrStats_idx=0
		tmrStats_Ypos=0
		tmrStats_increment=0.75
		tmrStats.Interval=6150
		tmrStats.Enabled=True 
	Else 
		tmrStats.Enabled=False 
		PuPlayer.LabelSet pDMDFull, "SplashImg1", "Backglass\\clear.png", 0, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':2, 'ypos':100, 'zback':1}"
		PuPlayer.LabelSet pDMDFull, "SplashImg2", "Backglass\\clear.png", 0, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':0, 'ypos':100, 'zback':1}"
		PuPlayer.LabelSet pDMDFull, "StatsName1", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore1", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "StatsName2", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore2", "", 1, ""
		PuPlayer.LabelSet pDMDFull, "StatsName3", "",  1, ""
		PuPlayer.LabelSet pDMDFull, "StatsScore3", "", 1, ""
	End if 

End Sub

Dim tmrStats2_idx
Dim tmrStats_idx
Dim tmrStats_Ypos
Dim tmrStats_YPos_BG
Dim tmrStats_increment
Sub tmrStats_Timer()
	Dim txtNames(6)
	Dim txtScores(6)
	Dim rowCount:rowCount=2

	If tmrStats2_idx>=30 then 
		TimerStats False
		Exit Sub 
	End if 
'WriteToLog "     ", "tmrStats_Timer:" & tmrStats_Ypos
	If tmrStats_idx=0 then
		tmrStats.Interval=20

		If tmrStats_Ypos<-76 then 
			PuPlayer.LabelSet pDMDFull, "Splash1", " ",	1, ""
			PuPlayer.LabelSet pDMDFull, "Splash2", " ", 1, ""
		Else 
			PuPlayer.LabelSet pDMDFull, "Splash1", HighScoreName(0),		  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+78 & ", 'color':" & pupColorBrown & "}"
			PuPlayer.LabelSet pDMDFull, "Splash2", FormatScore(HighScore(0)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+90 & ", 'color':" & pupColorBrown & "}"
		End if 

		PuPlayer.LabelSet pDMDFull, "StatsName1", HighScoreName(1),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+130 & ", 'color':" & pupColorBrown & "}"
		PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(HighScore(1)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+143 & ", 'color':" & pupColorBrown & "}"

		PuPlayer.LabelSet pDMDFull, "StatsName2", HighScoreName(2),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+180 & ", 'color':" & pupColorBrown & "}"
		PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(HighScore(2)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+193 & ", 'color':" & pupColorBrown & "}"

		PuPlayer.LabelSet pDMDFull, "SplashImg1", "", 1, "{'mt':2,'ypos':" &  (100+tmrStats_Ypos) & "}"
		PuPlayer.LabelSet pDMDFull, "SplashImg2", "", 1, "{'mt':2,'ypos':" &  (100+tmrStats_Ypos) & "}"

		tmrStats_Ypos=tmrStats_Ypos-tmrStats_increment
		if tmrStats_Ypos<-100 or tmrStats_Ypos>100 then
			tmrStats_idx=1
			tmrStats_Ypos=0
			tmrStats_increment=0.75
'			tmrStats.Interval=6150
			tmrStats.Enabled=False 

			PuPlayer.LabelSet pDMDFull, "SplashImg1", "Backglass\\Stone" & tmrStats_idx +1 & ".png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':2, 'ypos':100, 'zback':1}"
			PuPlayer.LabelSet pDMDFull, "SplashImg2", "Backglass\\Stone" & tmrStats_idx +2 & ".png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':0, 'ypos':100, 'zback':1}"

		End if 
	Else
		tmrStats.Interval=20
		If tmrStats_idx=1 then ' High Scores 
			txtNames(0)=HighScoreName(1):txtScores(0)=HighScore(1)
			txtNames(1)=HighScoreName(2):txtScores(1)=HighScore(2)
			txtNames(2)=HighScoreName(3):txtScores(2)=HighScore(3)
			txtNames(3)=HighScoreName(4):txtScores(3)=HighScore(4)
		Elseif tmrStats_idx=2 then 
'			tmrStats_increment=0.75*4
			tmrStats.Interval=7
			rowCount=2.5
			txtNames(0)=HighScoreName(3):txtScores(0)=HighScore(3)
			txtNames(1)=HighScoreName(4):txtScores(1)=HighScore(4)
			txtNames(2)=StatName(tmrStats2_idx):txtScores(2)=StatScore(tmrStats2_idx)
			txtNames(3)=StatName(tmrStats2_idx+1):txtScores(3)=StatScore(tmrStats2_idx+1)
			txtNames(4)=StatName(tmrStats2_idx+2):txtScores(4)=StatScore(tmrStats2_idx+2)
		Elseif tmrStats_idx=12 then 		' Show last Maiden one
			rowCount=4
			tmrStats.Interval=7
			txtNames(0)=StatName(tmrStats2_idx):txtScores(0)=StatScore(tmrStats2_idx)
			txtNames(1)=StatName(tmrStats2_idx+1):txtScores(1)=StatScore(tmrStats2_idx+1)
			txtNames(2)=StatName(tmrStats2_idx+2):txtScores(2)=StatScore(tmrStats2_idx+2)
			txtNames(3)="":txtScores(3)=""
			txtNames(4)="":txtScores(4)=""
			txtNames(5)="":txtScores(5)=""
		Else
			rowCount=3
			tmrStats.Interval=7
			txtNames(0)=StatName(tmrStats2_idx):txtScores(0)=StatScore(tmrStats2_idx)
			txtNames(1)=StatName(tmrStats2_idx+1):txtScores(1)=StatScore(tmrStats2_idx+1)
			txtNames(2)=StatName(tmrStats2_idx+2):txtScores(2)=StatScore(tmrStats2_idx+2)
			txtNames(3)=StatName(tmrStats2_idx+3):txtScores(3)=StatScore(tmrStats2_idx+3)
			txtNames(4)=StatName(tmrStats2_idx+4):txtScores(4)=StatScore(tmrStats2_idx+4)
			txtNames(5)=StatName(tmrStats2_idx+5):txtScores(5)=StatScore(tmrStats2_idx+5)
		End if 

		if (100+tmrStats_Ypos)>=0 then 
			PuPlayer.LabelSet pDMDFull, "SplashImg1", "", 1, "{'mt':2,'ypos':" &  (100+tmrStats_Ypos) & "}"
			PuPlayer.LabelSet pDMDFull, "SplashImg2", "", 1, "{'mt':2,'ypos':" &  (100+tmrStats_Ypos) & "}"
		End if 

		' 2 ROWS 
		if rowCount=2 then 
			If tmrStats_Ypos>-30 then 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(0),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+30 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(0)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+42 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(2),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+130 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(2)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+142 & ", 'color':" & pupColorBrown & "}"
			End if 
			
			If tmrStats_Ypos>-80 then 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(1),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+80 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(1)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+92 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(3),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+180 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(3)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+193 & ", 'color':" & pupColorBrown & "}"
			End if
		' 2 Rows out 3 Rowns in
		elseif rowCount=2.5 then 
			If tmrStats_Ypos>-25 then 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(0),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+30 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(0)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+42 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(2),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+125 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(2)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+135 & ", 'color':" & pupColorBrown & "}"
			End if 
			
			If tmrStats_Ypos>-58 then 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(1),			  1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+80 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(1)), 1, "{'mt':2, 'size':9, 'ypos':" & tmrStats_Ypos+92 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(3),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+158 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(3)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+168 & ", 'color':" & pupColorBrown & "}"
			End if 

			If tmrStats_Ypos<=-90 then 
				PuPlayer.LabelSet pDMDFull, "StatsName3", txtNames(4),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+190 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore3", FormatScore(txtScores(4)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+200 & ", 'color':" & pupColorBrown & "}"
			End if 

		' 3 ROWS out 3 Rowns IN
		elseif rowCount=3 then 
			If tmrStats_Ypos>-25 then 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(0),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+25 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(0)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+35 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(3),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+125 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(3)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+135 & ", 'color':" & pupColorBrown & "}"
			End if 
			
			If tmrStats_Ypos>-58 then 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(1),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+58 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(1)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+68 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(4),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+158 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(4)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+168 & ", 'color':" & pupColorBrown & "}"
			End if 

			If tmrStats_Ypos>-90 then 
				PuPlayer.LabelSet pDMDFull, "StatsName3", txtNames(2),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+90 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore3", FormatScore(txtScores(2)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+100 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName3", txtNames(5),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+190 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore3", FormatScore(txtScores(5)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+200 & ", 'color':" & pupColorBrown & "}"
			End if 

		elseif rowCount=4 then 			' Last One 
			If tmrStats_Ypos>-25 then 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(0),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+25 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", FormatScore(txtScores(0)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+35 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName1", txtNames(3),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+125 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore1", "", 					  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+135 & ", 'color':" & pupColorBrown & "}"
			End if 
			
			If tmrStats_Ypos>-58 then 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(1),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+58 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", FormatScore(txtScores(1)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+68 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName2", txtNames(4),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+158 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore2", "", 					  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+168 & ", 'color':" & pupColorBrown & "}"
			End if 

			If tmrStats_Ypos>-90 then 
				PuPlayer.LabelSet pDMDFull, "StatsName3", txtNames(2),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+90 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore3", FormatScore(txtScores(2)), 1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+100 & ", 'color':" & pupColorBrown & "}"
			Else 
				PuPlayer.LabelSet pDMDFull, "StatsName3", txtNames(5),			  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+190 & ", 'color':" & pupColorBrown & "}"
				PuPlayer.LabelSet pDMDFull, "StatsScore3", "", 					  1, "{'mt':2, 'size':6.5, 'ypos':" & tmrStats_Ypos+200 & ", 'color':" & pupColorBrown & "}"
			End if 

		End if 
		tmrStats_Ypos=tmrStats_Ypos-tmrStats_increment

		if tmrStats_Ypos<-100 or tmrStats_Ypos>100 then
			tmrStats_increment=0.75
			tmrStats_idx=tmrStats_idx+1
			tmrStats_Ypos=0
			if tmrStats_idx>3 then tmrStats2_idx=tmrStats2_idx+3

			PuPlayer.LabelSet pDMDFull, "SplashImg1", "Backglass\\Stone" & tmrStats_idx +1 & ".png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':2, 'ypos':100}"
			PuPlayer.LabelSet pDMDFull, "SplashImg2", "Backglass\\Stone" & tmrStats_idx +2 & ".png", 1, "{'mt':2,'width':100,'height':100, 'xalign':0, 'yalign':0, 'ypos':100}"

			' Delay between scroll is not consistent 
'			tmrStats.Interval=6150
			tmrStats.Enabled=False 
'			if tmrStats_idx=6 then tmrStats.Interval=6600
'			if tmrStats_idx=8 then tmrStats.Interval=6000

WriteToLog "     ", "tmrStats_idx:" & tmrStats_idx & " tmrStats2_idx:" & tmrStats2_idx
		End if 

	End if 

'WriteToLog "     ", "tmrStats_Ypos:" & tmrStats_Ypos
End Sub  


Function Max(value, minVal)
	if value > minVal then 
		Max=minVal
	Else 
		Max=Value
	End If 

End Function

'************************ called during gameplay to update Scores ***************************
'************************ called during gameplay to update Scores ***************************
Dim pUpdateScoreIndex:pUpdateScoreIndex=0
Sub pUpdateScores
	Dim PlayerName
	if pDMDCurPage <> pScores then Exit Sub

'New code has been added to display mutliplayer display on pupdmd - Chris
Dim NextPlayer:NextPlayer = CurrentPlayer + 1
	If bShowMatch=False then 
'		if bFreePlay then 
'			puPlayer.LabelSet pDMDFull,"Credits","FREE PLAY " ,1,""	
'		Else 
'			puPlayer.LabelSet pDMDFull,"Credits","CREDITS " & ""& Credits ,1,""
'		End if 

'		puPlayer.LabelSet pDMDFull,"Ball","BALL " & ""& balls ,1,""
	End if 

	' Set the score and color of the other players score 
	Select case PlayersPlayingGame
		case 1:
		case 2:
			If Currentplayer<>0 and ScoreSave(0)<>Score(0) then PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':38}"
			If Currentplayer<>1 and ScoreSave(1)<>Score(1) then PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':62}"
		case 3:
			If Currentplayer<>0 and ScoreSave(0)<>Score(0) then PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':25}"
			If Currentplayer<>1 and ScoreSave(1)<>Score(1) then PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':50}"
			If Currentplayer<>2 and ScoreSave(2)<>Score(2) then PuPlayer.LabelSet pDMDFull,"ScorePos3",FormatScore(Score(2)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':75}"
		case 4:
			If Currentplayer<>0 and ScoreSave(0)<>Score(0) then PuPlayer.LabelSet pDMDFull,"ScorePos1",FormatScore(Score(0)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':13}"
			If Currentplayer<>1 and ScoreSave(1)<>Score(1) then PuPlayer.LabelSet pDMDFull,"ScorePos2",FormatScore(Score(1)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':38}"
			If Currentplayer<>2 and ScoreSave(2)<>Score(2) then PuPlayer.LabelSet pDMDFull,"ScorePos3",FormatScore(Score(2)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':63}"
			If Currentplayer<>3 and ScoreSave(3)<>Score(3) then PuPlayer.LabelSet pDMDFull,"ScorePos4",FormatScore(Score(3)),1,"{'mt':2, 'color':" & RGB(255,157,6) &", 'xpos':87}"
	End Select

	if PlayersPlayingGame=1 then
		if ScoreSave(0)<>Score(0) then 
			puPlayer.LabelSet pDMDFull,"CurrentPlayerScore", FormatScore(Score(CurrentPlayer))	 ,1,""
			PuPlayer.LabelSet pDMDFull,"ScorePos1", "", 1,"{'mt':2, 'color':" & RGB(255,255,255) &"}"
		End if 
	Else 

		if ScorbitActive then 
			if Scorbit.bSessionActive then
				PlayerName=Scorbit.GetName(CurrentPlayer+1)
				if PlayerName="" then PlayerName= "Player " & CurrentPlayer+1 
			End if 
		End if 

		pUpdateScoreIndex=pUpdateScoreIndex+1
		puPlayer.LabelSet pDMDFull,"CurrentPlayerScore", FormatScore(Score(CurrentPlayer))	,1,"{'mt':2, 'yalign':0, 'ypos':81}"

		' Make it Flicker "Up The Irons!"
		if pUpdateScoreIndex>308 then
			pUpdateScoreIndex=0
			PuPlayer.LabelSet pDMDFull,"ScorePos" & CurrentPlayer+1, PlayerName, 1,"{'mt':2, 'color':" & RGB(255,255,255) &"}"
		elseif pUpdateScoreIndex>300 then
			PuPlayer.LabelSet pDMDFull,"ScorePos" & CurrentPlayer+1, "Up The Irons!", 1,"{'mt':2, 'color':" & pupColorRed &"}"
		elseif pUpdateScoreIndex>292 then 
			PuPlayer.LabelSet pDMDFull,"ScorePos" & CurrentPlayer+1, PlayerName, 1,"{'mt':2, 'color':" & pupColorRed &"}"
		Else 
			PuPlayer.LabelSet pDMDFull,"ScorePos" & CurrentPlayer+1, PlayerName, 1,"{'mt':2, 'color':" & RGB(255,255,255) &"}"
		End if 
	End if 

	ScoreSave(0)=Score(0)
	ScoreSave(1)=Score(1)
	ScoreSave(2)=Score(2)
	ScoreSave(3)=Score(3)

end Sub


Sub PowerFeaturesFlash(bEnable )
	if bEnable then
		if tmrFlashPF.Enabled=False then 
			tmrFlashPF.Interval = 150
			tmrFlashPF.UserValue = False
			tmrFlashPF.Enabled=True 
		End if 
	else
		tmrFlashPF.UserValue=False 
		tmrFlashPF.Enabled=False
		tmrFlashPF_Timer			' - clear 
	End if 
End Sub 

Sub tmrFlashPF_Timer()
	tmrFlashPF.UserValue=Not tmrFlashPF.UserValue

	if bPowerSpinners2(CurrentPlayer)=2 then
		if tmrFlashPF.UserValue then 
			puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':3.8,'zback':1}"
'			puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Lit.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':6,'zback':1}"
		else 
			puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\PF-Spin-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':6,'zback':1}"
		End if 
	End if 

	if bPowerRamps2(CurrentPlayer)=2 then
		if tmrFlashPF.UserValue then 
			puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':16.8,'zback':1}"
		else 
			puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\PF-Ramps-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':19,'zback':1}"
		End if 
	End if 

	if bPowerPops2(CurrentPlayer)=2 then
		if tmrFlashPF.UserValue then 
			puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':30,'zback':1}"
		else 
			puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\PF-Pops-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':32,'zback':1}"
		End if 
	End if 

	if bPowerTargets2(CurrentPlayer)=2 then
		if tmrFlashPF.UserValue then 
			puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':43,'zback':1}"
		else 
			puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\PF-Targ-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':45,'zback':1}"
		End if 
	End if 

	if bPowerOrbits2(CurrentPlayer)=2 then
		if tmrFlashPF.UserValue then 
			puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Lit.png",1,"{'mt':2,'color':0,'width':8.2, 'height':15.2, 'xpos':1.8,'ypos':55.8,'zback':1}"
		else 
			puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\PF-Orb-Reg.png",1,"{'mt':2,'color':0,'width':6, 'height':11, 'xpos':3,'ypos':58,'zback':1}"
		End if 
	End if 
	
End Sub 

Sub pPowerFeatures
Dim Value 
Dim bSkipVisual:bSkipVisual=False

	if IsModeActive(kModeRTTH) then Exit Sub				' Dont update this During RTTH
	if IsModeActive(kModeCyborg) then Exit Sub				' Dont update this During Cyborg
	if IsModeActive(kMode2M2M) then bSkipVisual=True 
	if IsModeActive(kModeNOTB) then bSkipVisual=True

	
	' display the remaining Power Feature counts required to light SJP
	' but when the countdown gets to zero, restart the count at level 2 with colour pyramids
	' SPINNERS *******************************************************

	Value = " "
	if bPowerSpinners2(CurrentPlayer)<=1 then		' Phase1 
		If PowerSpinnersCount(CurrentPlayer) < 40*CyborgDifficulty(CurrentPlayer) Then
			Value=(40*CyborgDifficulty(CurrentPlayer) - PowerSpinnersCount(CurrentPlayer))
		elseif PowerSpinnersCount(CurrentPlayer) < 40*CyborgDifficulty(CurrentPlayer)+40 then 
			Value=(40*CyborgDifficulty(CurrentPlayer)+40 - PowerSpinnersCount(CurrentPlayer))
		End if 
	End if 
	' Dont update if we are animating 
	if tmrPowerStart(0).Enabled=False and bSkipVisual=False then PuPlayer.LabelSet pDMDFull,"RemainingSpinners" ,Value, 1,"{'mt':2,'xpos':6,'ypos':10.4}"

	' POPS *******************************************************
	Value = " "
	if bPowerPops2(CurrentPlayer)<=1 then		' Phase1 
		If PowerPopsCount(CurrentPlayer) < 20*CyborgDifficulty(CurrentPlayer) Then
			Value=(20*CyborgDifficulty(CurrentPlayer) - PowerPopsCount(CurrentPlayer))
		elseif PowerPopsCount(CurrentPlayer) < 20*CyborgDifficulty(CurrentPlayer)+20 then 
			Value=(20*CyborgDifficulty(CurrentPlayer)+20 - PowerPopsCount(CurrentPlayer))
		End if 
	End if 
	' Dont update if we are animating 
	if tmrPowerStart(1).Enabled=False and bSkipVisual=False then PuPlayer.LabelSet pDMDFull,"RemainingPops",Value,1,"{'mt':2,'xpos':6,'ypos':36.6}"

	' RAMPS *******************************************************
	Value = " "
	if bPowerRamps2(CurrentPlayer)<=1 then		' Phase1 
		If PowerRampsCount(CurrentPlayer) < 8*CyborgDifficulty(CurrentPlayer) Then
			Value=(8*CyborgDifficulty(CurrentPlayer) - PowerRampsCount(CurrentPlayer))
		elseif PowerRampsCount(CurrentPlayer) < 8*CyborgDifficulty(CurrentPlayer)+8 then 
			Value=(8*CyborgDifficulty(CurrentPlayer)+8 - PowerRampsCount(CurrentPlayer))
		End if 
	End if 
	' Dont update if we are animating 
	if tmrPowerStart(2).Enabled=False and bSkipVisual=False then PuPlayer.LabelSet pDMDFull,"RemainingRamps",Value,1,"{'mt':2,'xpos':6,'ypos':23.7}"

	' TARGETS *******************************************************
	Value = " "
	if bPowerTargets2(CurrentPlayer)<=1 then		' Phase1 
		If PowerTargetsCount(CurrentPlayer) < 12*CyborgDifficulty(CurrentPlayer) Then
			Value=(12*CyborgDifficulty(CurrentPlayer) - PowerTargetsCount(CurrentPlayer))
		elseif PowerTargetsCount(CurrentPlayer) < 12*CyborgDifficulty(CurrentPlayer)+12 then 
			Value=(12*CyborgDifficulty(CurrentPlayer)+12 - PowerTargetsCount(CurrentPlayer))
		End if 
	End if 
	' Dont update if we are animating 
	if tmrPowerStart(3).Enabled=False and bSkipVisual=False then PuPlayer.LabelSet pDMDFull,"RemainingTargets",Value,1,"{'mt':2,'xpos':6,'ypos':49.6}"

	' ORBITS *******************************************************
	Value = " "
	if bPowerOrbits2(CurrentPlayer)<=1 then		' Phase1 
		If PowerOrbitsCount(CurrentPlayer) < 10*CyborgDifficulty(CurrentPlayer) Then
			Value=(10*CyborgDifficulty(CurrentPlayer) - PowerOrbitsCount(CurrentPlayer))
		elseif PowerOrbitsCount(CurrentPlayer) < 10*CyborgDifficulty(CurrentPlayer)+10 then 
			Value=(10*CyborgDifficulty(CurrentPlayer)+10 - PowerOrbitsCount(CurrentPlayer))
		End if 
	End if 
	' Dont update if we are animating 
	if tmrPowerStart(4).Enabled=False and bSkipVisual=False then PuPlayer.LabelSet pDMDFull,"RemainingOrbits",Value,1,"{'mt':2,'xpos':6,'ypos':62.7}"


'	' if the SJP is claimed at the Clairvoyant's ORB while a Power Feature is Ready, check it off as complete
'	If lPowerSpinner.state = 1 Then
'		puPlayer.LabelSet pDMDFull,"pSpinnersChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
'	End If
'	If lPowerPops.state = 1 Then
'		puPlayer.LabelSet pDMDFull,"pPopsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
'	End If
'	If lPowerOrbits.state = 1 Then
'		puPlayer.LabelSet pDMDFull,"pOrbitsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
'	End If
'	If lPowerRamps.state = 1 Then
'		puPlayer.LabelSet pDMDFull,"pRampsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
'	End If
'	If lPowerTargets.state = 1 Then
'		puPlayer.LabelSet pDMDFull,"pTargetsChk","PuPOverlays\\pCheckmark.gif",1,"{'mt':2,'color':0,'width':7, 'height':11, 'anigif':100,'pagenum':1}"
'	End If

	'Once all Power Features have been claimed via SJP hit then clear checkmarks
	If GetLightState(kModeMISC, kLightPowerPops)=0 AND GetLightState(kModeMISC, kLightPowerTargets)=0 AND GetLightState(kModeMISC, kLightPowerSpinners) = 0 AND GetLightState(kModeMISC, kLightPowerOrbits) = 0 AND GetLightState(kModeMISC, kLightPowerRamps) = 0 Then
		puPlayer.LabelSet pDMDFull,"pSpinnersChk","PuPOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMDFull,"pPopsChk","PuPOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMDFull,"pOrbitsChk","PuPOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMDFull,"pRampsChk","PuPOverlays\\clear.png",1,""
		puPlayer.LabelSet pDMDFull,"pTargetsChk","PuPOverlays\\clear.png",1,""
	End If
End Sub

Sub CheckPowerSpinner(mult)
	if isModeActive(kModeNOTB) then exit sub

	if IsModeQual(kModeCyborg) then 
		if GetLightState(kModeMISC, kLightPowerSpinners)<>1 then FlashLightning		' First time
		SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 1
		lPowerSpinnerArrow1.State = 1
		lPowerSpinnerArrow2.State = 1
		CheckCyborg
	ElseIf bPowerSpinners2(CurrentPlayer)<=1 Then
        PowerSpinnersCount(CurrentPlayer) = PowerSpinnersCount(CurrentPlayer) + 1
		if GetLightState(kModeMISC, kLightPowerSpinners)=0 then FlashForMS lPowerSpinner, 1000, 50, 0 

        If PowerSpinnersCount(CurrentPlayer) = 40*CyborgDifficulty(CurrentPlayer) Then ' if you hit 40 spinners
'WriteToLog "     ", "Spinner1 Done"
            bPowerSpinners2(CurrentPlayer)=1 ' Phase1 Done
            UpdatePowerFeature
			'pupevent 622
'			QueueScene "pupevent 622", 3000, 2
			PlaySoundVol "vo_powerspinners", VolDef
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPSpin.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2


'			AddJP cPowerFeatureValue, 1500000
			'bPJActivated = True
		ElseIf PowerSpinnersCount(CurrentPlayer) = 40*CyborgDifficulty(CurrentPlayer)+40 Then ' You hit 80 spinners (2nd Phase)
'WriteToLog "     ", "Spinner2 Done"
			QueueScene "PlaySoundVol ""vo_PSpinnersCompleted"", VolDef", 1, 2
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPowerJPLit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			AddJP cPowerJackpotMultiplier, 1
			AddJP cPowerFeatureValue, 1000000 * CyborgDifficulty(CurrentPlayer)
            bPowerSpinners2(CurrentPlayer)=2 ' Phase2 Done 
            UpdatePowerFeature
		End if 

        If PowerSpinnersCount(CurrentPlayer)> 40*CyborgDifficulty(CurrentPlayer) Then
			AddJP cPowerFeatureValue, (50000*mult) * CyborgDifficulty(CurrentPlayer)
        End If
		pPowerFeatures
    End If
End Sub


Sub CheckPowerPops()
	if isModeActive(kModeNOTB) then exit sub

	' active on all modes but Cyborg
WriteToLog "     ", "CheckPowerPops " & bPowerPops2(CurrentPlayer)

	if IsModeQual(kModeCyborg) then 
		if GetLightState(kModeMISC, kLightPowerPops)<>1 then FlashLightning		' First time

		SSetLightColor kModeMISC, kLightPowerPops, noColor, 1
		CheckCyborg
	ElseIf bPowerPops2(CurrentPlayer)<=1 Then 'this if it is not finished
		PowerPopsCount(CurrentPlayer) = PowerPopsCount(CurrentPlayer) + 1
		if GetLightState(kModeMISC, kLightPowerPops)=0 then FlashForMS lPowerPops, 1000, 50, 0 

WriteToLog "     ", "Pops " & PowerPopsCount(CurrentPlayer)

		If PowerPopsCount(CurrentPlayer) = 20 * CyborgDifficulty(CurrentPlayer) Then
WriteToLog "     ", "Pops Done"
			bPowerPops2(CurrentPlayer) = 1 'Phase1 Done
			UpdatePowerFeature
			'pupevent 620
'			QueueScene "pupevent 620", 3000, 2
			PlaySoundVol "vo_powerpops", VolDef
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPPop.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			Select case Balls
				Case 1:DMD "", "", "DMD_superb", eNone, eNone, eBlink, 1500, True, "":PlaySoundVol "vo_bumpers", VolDef
				Case 2:DMD "", "", "DMD_superb", eNone, eNone, eBlink, 1500, True, "":PlaySoundVol "vo_superbumperarelit", VolDef
				Case 3:DMD "", "", "DMD_superb", eNone, eNone, eBlink, 1500, True, "":PlaySoundVol "vo_superbumperarelit2", VolDef
			End Select
			
'			AddJP cPowerFeatureValue, 1500000 * CyborgDifficulty(CurrentPlayer)
			
			'bPJActivated = True
		ElseIf PowerPopsCount(CurrentPlayer) = 20 * CyborgDifficulty(CurrentPlayer)+20 Then ' You hit 40 spinners (2nd Phase)
			QueueScene "PlaySoundVol ""vo_PPopsCompleted"", VolDef", 1, 2
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPowerJPLit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			AddJP cPowerJackpotMultiplier, 1
			AddJP cPowerFeatureValue, 1000000 * CyborgDifficulty(CurrentPlayer)
            bPowerPops2(CurrentPlayer)=2 ' Phase2 Done 
            UpdatePowerFeature
		End if 

		If PowerPopsCount(CurrentPlayer)> 20 * CyborgDifficulty(CurrentPlayer) Then 'add more points to the SP jackpot
			AddJP cPowerFeatureValue, 100000 * CyborgDifficulty(CurrentPlayer)
		End If
		pPowerFeatures
    End If
End Sub 

Sub CheckPowerRamps()
	if isModeActive(kModeNOTB) then exit sub

	' active on all modes but Cyborg
    if IsModeQual(kModeCyborg) then 
		if GetLightState(kModeMISC, kLightPowerRamps)<>1 then FlashLightning		' First time

		SSetLightColor kModeMISC, kLightPowerRamps, noColor, 1
		lRampArrow.State = 1
		CheckCyborg
	ElseIf Mode(CurrentPlayer, 0) <> 3 AND bPowerRamps2(CurrentPlayer) <= 1 Then 'this if it is not finished
        PowerRampsCount(CurrentPlayer) = PowerRampsCount(CurrentPlayer) + 1
		if GetLightState(kModeMISC, kLightPowerRamps)=0 then FlashForMS lPowerRamps, 1000, 50, 0 

        If PowerRampsCount(CurrentPlayer) = 8 * CyborgDifficulty(CurrentPlayer) Then 'if ramps count = 8
            bPowerRamps2(CurrentPlayer) = 1 'this means ramps power feature is activated
			'pupevent 621
'			QueueScene "pupevent 621", 3000, 2
			PlaySoundVol "vo_powerramps", VolDef
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPRamp.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2


            DMD "", "", "DMD_Superr", eNone, eNone, eBlink, 1500, True, ""
            UpdatePowerFeature
'			AddJP cPowerFeatureValue, 1500000 * CyborgDifficulty(CurrentPlayer)
			'bPJActivated = True
        ElseIf PowerRampsCount(CurrentPlayer) = 8 * CyborgDifficulty(CurrentPlayer)+8 Then ' You hit 16 spinners (2nd Phase)
			QueueScene "PlaySoundVol ""vo_PRampsCompleted"", VolDef", 1, 2
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPowerJPLit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			AddJP cPowerJackpotMultiplier, 1
			AddJP cPowerFeatureValue, 1000000 * CyborgDifficulty(CurrentPlayer)

            bPowerRamps2(CurrentPlayer)=2 ' Phase2 Done 
            UpdatePowerFeature
		End if 

		If PowerRampsCount(CurrentPlayer)> 8 * CyborgDifficulty(CurrentPlayer) Then 'add more points to the SP jackpot
			AddJP cPowerFeatureValue, 125000 * CyborgDifficulty(CurrentPlayer)
        End If
		pPowerFeatures
    End If
End Sub 

Sub CheckPowerTargets()
	if isModeActive(kModeNOTB) then exit sub

WriteToLog "     ", "CheckPowerTargets: " & PowerTargetsCount(CurrentPlayer)
	' active on all modes but Cyborg
	if IsModeQual(kModeCyborg) then 
		if GetLightState(kModeMISC, kLightPowerTargets)<>1 then FlashLightning		' First time

		SSetLightColor kModeMISC, kLightPowerTargets, noColor, 1
		CheckCyborg
	ElseIf Mode(CurrentPlayer, 0) <> 3 AND bPowerTargets2(CurrentPlayer) <= 1 Then 'this if powertarget goal isn't reached
        PowerTargetsCount(CurrentPlayer) = PowerTargetsCount(CurrentPlayer) + 1
		if GetLightState(kModeMISC, kLightPowerTargets)=0 then FlashForMS lPowerTargets, 1000, 50, 0 


        If PowerTargetsCount(CurrentPlayer) = 12 * CyborgDifficulty(CurrentPlayer) Then
            bPowerTargets2(CurrentPlayer) = 1 'this means it is activated colour pyramid and PJ is ready
			'pupevent 623
'			QueueScene "pupevent 623", 3000, 2
			PlaySoundVol "vo_powertargets", VolDef
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPTarget.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

            DMD "", "", "DMD_Supert", eNone, eNone, eBlink, 1500, True, ""
'			AddJP cPowerFeatureValue, 150000 * CyborgDifficulty(CurrentPlayer)
            UpdatePowerFeature
			'bPJActivated = True
		ElseIf PowerTargetsCount(CurrentPlayer) = 12 * CyborgDifficulty(CurrentPlayer)+12 Then ' You hit 40 spinners (2nd Phase)

			QueueScene "PlaySoundVol ""vo_PTargetsCompleted"", VolDef", 1, 2
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPowerJPLit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			AddJP cPowerJackpotMultiplier, 1
			AddJP cPowerFeatureValue, 1000000 * CyborgDifficulty(CurrentPlayer)

            bPowerTargets2(CurrentPlayer)=2 ' Phase2 Done 
            UpdatePowerFeature
		End if 

		If PowerTargetsCount(CurrentPlayer)> 12 * CyborgDifficulty(CurrentPlayer) Then 'add more points to the SP jackpot if goal has been reached
			AddJP cPowerFeatureValue, 150000 * CyborgDifficulty(CurrentPlayer)
        End If
		pPowerFeatures
    End If
End sub 

Sub CheckPowerOrbits()
'WriteToLog "     ", "CheckPowerOrbits:" & IsModeQual(kModeCyborg) & " " & isModeActive(kModeNOTB)
	if isModeActive(kModeNOTB) then exit sub

	' active on all modes but Cyborg
	if IsModeQual(kModeCyborg) then 
		if GetLightState(kModeMISC, kLightPowerOrbits)<>1 then FlashLightning		' First time

		SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 1
		lOrbitArrow.State = 1
		lOrbitArrow2.State = 1
		CheckCyborg
	ElseIf Mode(CurrentPlayer, 0) <> 3 AND bPowerOrbits2(CurrentPlayer) <= 1 Then 'this if it is not finished
        PowerOrbitsCount(CurrentPlayer) = PowerOrbitsCount(CurrentPlayer) + 1
		if GetLightState(kModeMISC, kLightPowerOrbits)=0 then FlashForMS lPowerOrbits, 1000, 50, 0 

        If PowerOrbitsCount(CurrentPlayer) = 10 * CyborgDifficulty(CurrentPlayer) Then
            bPowerOrbits2(CurrentPlayer) = 1 'this means it is activated
			'pupevent 619
'			QueueScene "pupevent 619", 3000, 2
			PlaySoundVol "vo_powerorbits", VolDef
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPOrbit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2


            DMD "", "", "DMD_Supero", eNone, eNone, eBlink, 1500, True, ""
            UpdatePowerFeature
'			AddJP cPowerFeatureValue, 1500000 * CyborgDifficulty(CurrentPlayer)
			'bPJActivated = True
        ElseIf PowerOrbitsCount(CurrentPlayer) = 10 * CyborgDifficulty(CurrentPlayer)+10 Then ' You hit 20 orbits (2nd Phase)

			QueueScene "PlaySoundVol ""vo_POrbitsCompeted"", VolDef", 1, 2
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""PJP.mp4"", ""I:Callouts\\txtPowerJPLit.png^^^^^^^^^"", ""^^^^^^^^^"" ", 3000, 2
			QueueScene "SceneClearLabels", 0, 2

			AddJP cPowerJackpotMultiplier, 1
			AddJP cPowerFeatureValue, 1000000 * CyborgDifficulty(CurrentPlayer)
            bPowerOrbits2(CurrentPlayer)=2 ' Phase2 Done 
            UpdatePowerFeature
		End if 

		If PowerOrbitsCount(CurrentPlayer)> 10 * CyborgDifficulty(CurrentPlayer) Then 'add more points to the SP jackpot
			AddJP cPowerFeatureValue, 125000 * CyborgDifficulty(CurrentPlayer)
        End If
		pPowerFeatures
    End If
End Sub 

Sub ProcessModes(lIndex)

    If bPlayfieldValidated=False Then
		bPlayfieldValidated=True
        EnableBallSaver BallSaverTime
    End If

	ResetBallSearch()
	if bTableDisabled or bGameInPlay=False then exit sub

	ProcessLoops(lIndex)
	ProcessEddie(lIndex)
	ProcessFear(lIndex)
	ProcessAces(lIndex)
	ProcessRime(lIndex)
	ProcessHallowed(lIndex)
	ProcessIcarus(lIndex)
	Process2M2M(lIndex)

	ProcessRevive(lIndex)

	ProcessMummy(lIndex)
	ProcessTrooper(lIndex)
	ProcessMadness(lIndex)
	ProcessNOTB(lIndex)
	ProcessRTTH(lIndex)
End Sub


Sub ProcessRTTH(lIndex)
	Dim JPtxt
	if isModeActive(kModeRTTH)=False then exit sub
	WriteToLog "     ", "ProcessRTTH:" & lIndex 
	
	if lIndex = kLightSpinnerLeft or lIndex = kLightSpinnerRight then 					' Spinners 
		If lPowerSpinnerArrow1.State = 2 or lPowerSpinnerArrow2.State = 2 then 
			if lIndex = kLightSpinnerLeft then lPowerSpinnerArrow1.State=1
			if lIndex = kLightSpinnerRight then lPowerSpinnerArrow2.State=1
			
			AddJPMode kModeRTTH, 100000
			SetDefPulse(lPowerSpinner):SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 1
			if GetLightState(kModeMISC, kLightPowerTargets) = 0 then
				SetFastPulse(lPowerTargets)
				SSetLightColor kModeMISC, kLightPowerTargets, noColor, 2
			End if 
		Else 
			AddJPMode kModeRTTH, 60000
		End if 

	elseif lIndex = kLightBonusX or lIndex=kLightOrb or lIndex=kLightLock then 		' Targets 
		If GetLightState(kModeMISC, kLightPowerTargets) = 2 then 
			AddJPMode kModeRTTH, 100000
			SetDefPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 1
			if GetLightState(kModeMISC, kLightPowerPops)=0 then 
				SetFastPulse(lPowerPops)
				SSetLightColor kModeMISC, kLightPowerPops, noColor, 2
			End if 
		Else 
			AddJPMode kModeRTTH, 60000
		End if 

	elseif lIndex = kLightBumper then 													' Bumpers 
		If GetLightState(kModeMISC, kLightPowerPops) = 2 then 
			SetDefPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 1
			AddJPMode kModeRTTH, 100000
			
			if GetLightState(kModeMISC, kLightPowerOrbits)=0 then 
				SetFastPulse(lPowerOrbits)
				SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 2
				lOrbitArrow.State = 2
				lOrbitArrow2.State = 2
			End if 
		Else 
			AddJPMode kModeRTTH, 60000
		End if 

	elseif lIndex = kLightOrbitLeft or lIndex = kLightOrbitRight then 					' Orbits 
		If lOrbitArrow.State = 2 or lOrbitArrow2.State = 2 then 
			if lIndex = kLightOrbitLeft then lOrbitArrow.State=1
			if lIndex = kLightOrbitRight then lOrbitArrow2.State=1

			AddJPMode kModeRTTH, 100000
			SetDefPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 1
			if GetLightState(kModeMISC, kLightPowerRamps)=0 then 
				SetFastPulse(lPowerRamps)
				SSetLightColor kModeMISC, kLightPowerRamps, noColor, 2
				lRampArrow.State = 2
			End if 
		Else 
			AddJPMode kModeRTTH, 60000
		End if 

	elseif lIndex = kLightRampLeft or lIndex = kLightRampRight then 					' Ramps 
		If GetLightState(kModeMISC, kLightPowerRamps) = 2 then 
			SSetLightColor kModeMISC, kLightPowerRamps, noColor, 1
			lRampArrow.State = 1
			
			AddJPMode kModeRTTH, 100000
			if light014.State=0 then
				SetFastPulse light014:light014.state = 2	' Cyborg ? Light
				FlashLightning2

				StartRainbowMode kModeRTTH, kLightRampCenter
				lUnderworld.State = 2
				SetSlowPulse lUnderworld
				PharaohBullseyeFlasherEnabled True
			End if 
		Else 
			AddJPMode kModeRTTH, 60000
		End if 

	End if 

	If lIndex=kLightSpinnerLeft or lIndex=kLightSpinnerRight then exit sub   ' Dont use these because it floods with spins
	If lIndex=kLightSpinnerStop then lIndex=kLightSpinnerLeft 				 ' Use This Instead 

	if GetLightState(kModeRTTH , lIndex)<>0 then 
		if lIndex=kLightRampCenter then 												' Super JP
			PlaySoundVol "vo_superjackpot", VolDef
			SetDefPulse light072:light072.state = 0
			SetDefPulse light073:light073.state = 0 
			FlashLightning

			' Reset Everything 
			SetFastPulse(lPowerSpinner):SSetLightColor kModeMISC, kLightPowerSpinners, noColor, 2
			SetFastPulse(light014):light014.State = 0				' Red "?" Light at the top of Pyramid inserts
			SetFastPulse(lPowerPops):SSetLightColor kModeMISC, kLightPowerPops, noColor, 0
			SetFastPulse(lPowerTargets):SSetLightColor kModeMISC, kLightPowerTargets, noColor, 0
			SetFastPulse(lPowerOrbits):SSetLightColor kModeMISC, kLightPowerOrbits, noColor, 0
			SetFastPulse(lPowerRamps):SSetLightColor kModeMISC, kLightPowerRamps, noColor, 0

			' Set Power Triange inserts around PF
			lOrbitArrow.State = 0
			lRampArrow.State = 0
			lOrbitArrow2.State = 0
			lPowerSpinnerArrow1.State = 2
			lPowerSpinnerArrow2.State = 2

			StopRainbow 
			lUnderworld.State = 0
			PharaohBullseyeFlasherEnabled False 

			JPtxt=FormatScore(RTTH_SJP)
			AddScoreMode kModeRTTH, RTTH_SJP
	
			RTTH_JPX=RTTH_JPX+1										' Increase Multiplier 
			RTTH_SJP=0
			ModeJPPoints(CurrentPlayer, kModeRTTH)= 5000000 + ((RTTH_JPX-1)*1000000)

			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""wizRTTH"", ""RTTH" & INT(RND*5)+1 & ".mp4"", ""I:wizRTTH\\txtSJP.png^^^^^^^^" & JPtxt & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", 2000, 1
			QueueScene "SceneClearLabels", 0, 1

		else 																			' Jackpot 		
			Select case RTTH_JPX
				case 1 
					PlaySoundVol "vo_jackpot", VolDef
				case 2
					PlaySoundVol "vo_doublejackpot", VolDef
				case 2
					PlaySoundVol "vo_triplejackpot", VolDef
				case else 
					PlaySoundVol "vo_jackpot", VolDef
			End Select 

			JPtxt=FormatScore(ModeJPPoints(CurrentPlayer, kModeRTTH))
			AddScoreMode kModeRTTH, ModeJPPoints(CurrentPlayer, kModeRTTH)
			RTTH_SJP=RTTH_SJP+ModeJPPoints(CurrentPlayer, kModeRTTH)

			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""wizRTTH"", ""RTTH.mp4"", ""I:wizRTTH\\JP" & RTTH_JPX & ".png^^^^^^^^" & JPtxt & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", 2000, 1
			QueueScene "SceneClearLabels", 0, 1
		End if 
	End If

End Sub  


Sub ProcessNOTB(lIndex)
	Dim JPScore
	Dim JPtxt
	if isModeActive(kMode2M2M) then Exit Sub 							' Cant stack with 2M2M
	if isModeActive(kModeRTTH) then Exit Sub 							' Cant stack with RTTH
	if isModeActive(kModeCyborg) then Exit Sub 							' Cant stack with Cyborg

	if IsModeQual(kModeMummy) then Exit Sub								' Cant stack with Mummy
	if IsModeQual(kModeTrooper) then Exit Sub							' Cant stack with Trooper

	if isModeActive(kModeNOTB)=False then exit sub
WriteToLog "     ", "ProcessNOTB: " & lIndex 

	If IsModeActive(kModeNOTB) and IsModeQual(kModeNOTB)=False then 
		if lIndex=kLightOrbitLeft2 or lIndex=kLightOrbitRight2 then 
			StartNOTB
		End If
	ElseIf IsModeQual(kModeNOTB) Then 
		if GetLightState(kModeNOTB , lIndex)<>0 then 
'			PlaySoundVol "sfx_madnesHit1", VolSfx
'			PlaySoundVol "vo_MadnessJP" & INT(RND*2)+1, VolSfx

			if NOTBPhase = 1 then 
				QueueScene "SceneGeneralStartDef False, False, ""NOTB"", ""parry.mp4"", ""^^^^^^^^^"" ", 1, 1
				QueueScene "PlaySoundVol ""vo_parry" & INT(RND*3)+1 & """, VolSfx ", 1, 1 
				QueueScene "PlaySoundVol ""sfx_notb_hit"", VolSfx ", 3000, 1 
				QueueScene "SceneClearLabels", 0, 1

				AddScoreMode kModeNOTB, 1000000

				SSetLightColor kModeNOTB,kLightRampLeft  , White, 0
				SSetLightColor kModeNOTB,kLightLoopLeft  , White, 0
				SSetLightColor kModeNOTB,kLightOrbitLeft , White, 0
				SSetLightColor kModeNOTB,kLightRampCenter, Red, 2
				SSetLightColor kModeNOTB,kLightLoopRight , White, 0
				SSetLightColor kModeNOTB,kLightRampRight , White, 0
				SSetLightColor kModeNOTB,kLightOrbitRight, White, 0
				PharaohBullseyeFlasherEnabled True

				NOTBPhase = 2
				tmrNOTB.UserValue = 7			' Start 5 second timer +2second grace
				tmrNOTB.Interval = 1000
				tmrNOTB.Enabled = True 

			Elseif NOTBPhase = 2 then			' Hit the bullseye
				PharaohBullseyeFlasherEnabled False
				if BullseyeMultiplier=3 then 
					JPScore=6000000
				Elseif BullseyeMultiplier=2 then 
					JPScore=4000000
				Else 
					JPScore=2000000
				End if 
				JPtxt=FormatScore(JPScore)
				AddScoreMode kModeNOTB, JPScore
				NOTBAttackCount=NOTBAttackCount-1
				if NOTBAttackCount=0 then 
					EndNOTB(True)  ' Finish 
				else 
					QueueScene "PlaySoundVol ""sfx_notb_hit"", VolSfx ", 1, 1 
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""NOTB"", ""Counter.mp4"", ""I:NOTB\\Counter.png^^^^^^^^" & JPtxt & "^"", ""^^^^^^^^3000:" & pupColorRed & "^"" ", 3000, 1
					QueueScene "SceneClearLabels", 0, 1

					SSetLightColor kModeNOTB,kLightRampLeft  , yellow, 2
					SSetLightColor kModeNOTB,kLightLoopLeft  , yellow, 2
					SSetLightColor kModeNOTB,kLightOrbitLeft , yellow, 2
					SSetLightColor kModeNOTB,kLightRampCenter, white, 0
					SSetLightColor kModeNOTB,kLightLoopRight , yellow, 2
					SSetLightColor kModeNOTB,kLightRampRight , yellow, 2
					SSetLightColor kModeNOTB,kLightOrbitRight, yellow, 2

					NOTBPhase = 3
					tmrNOTB.UserValue = 17			' Start 15 second timer +2second grace
					tmrNOTB.Interval = 1000
					tmrNOTB.Enabled = True 
				End if 
			Elseif NOTBPhase = 3 then

				JPtxt=FormatScore(ModeJPPoints(CurrentPlayer, kModeNOTB))
				AddScoreMode kModeNOTB, ModeJPPoints(CurrentPlayer, kModeNOTB)
				AddJPMode kModeNOTB, 1000000
				NOTBAttackCount=NOTBAttackCount-1

				if NOTBAttackCount=0 then 
					EndNOTB(True) ' Finish
				else 
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""NOTB"", ""Counter.mp4"", ""I:NOTB\\Attack.png^^^^^^^^" & JPtxt & "^"", ""^^^^^^^^3000:" & pupColorRed & "^"" ", 1, 1
					QueueScene "PlaySoundVol ""sfx_notb_hit"", VolSfx ", 3000, 1
					QueueScene "SceneClearLabels", 0, 1
					SSetLightColor kModeNOTB,kLightRampLeft  , yellow, 2
					SSetLightColor kModeNOTB,kLightLoopLeft  , yellow, 2
					SSetLightColor kModeNOTB,kLightOrbitLeft , yellow, 2
					SSetLightColor kModeNOTB,kLightRampCenter, white, 0
					SSetLightColor kModeNOTB,kLightLoopRight , yellow, 2
					SSetLightColor kModeNOTB,kLightRampRight , yellow, 2
					SSetLightColor kModeNOTB,kLightOrbitRight, yellow, 2
					SSetLightColor kModeNOTB,lIndex, 			yellow, 0		' Turn off Current shot 
				End if 
			End if 
		End if 
	End if 
End Sub 


Sub CheckMadnessFreeze()		' Freeze and start the timer
	dim i
	if isModeActive(kModeMadness)=False then exit sub
	if tmrMadnessFreeze.Enabled then Exit Sub 	' Doesnt Extend does it ???
	
	For i = 0 to 7 
		if MadnessShots(i)=2 then 
WriteToLog "     ", "Locking"
			MadnessShots(i)=1
			SetMadnessLight i, MadnessShots(i)
		End if 
	Next
	tmrMadnessFreeze.Interval = 10000
	tmrMadnessFreeze.Enabled = True 
End Sub 

Sub tmrMadnessFreeze_Timer()	' Unfreeze and stop timer
	Dim i
	For i = 0 to 7 
		if MadnessShots(i)=1 then 
			MadnessShots(i)=2
			SetMadnessLight i, MadnessShots(i)
		End if 
	Next
	tmrMadnessFreeze.Enabled=False
End Sub 

Sub SetMadnessLightAll()
	Dim i
	For i = 0 to 7 
		SetMadnessLight i, MadnessShots(i)
	Next
End Sub 


Sub SetMadnessLight(index, state)
	Dim NewState:NewState=state
WriteToLog "     ", "SetMadnessLight:" & index & " " & state 
	dim color
	if MadnessShots(index)=-1 or MadnessShots(index)=0 then			' Off
		NewState=0
		color=red
	elseif MadnessShots(index)=1 then		' Red Solid 
		color=red
	elseif MadnessShots(index)=2 then		' Red Flash
		color=red
	elseif MadnessShots(index)=3 then		' Green Solid
		color=green
		NewState=1
	elseif MadnessShots(index)=4 then		' Green Flash
		color=green
		NewState=2
	End if

	Select case index 
		case 0:
			SSetLightColor kModeMadness, kLightSpinnerLeft, color, NewState
		case 1:
			SSetLightColor kModeMadness, kLightRampLeft, color, NewState
		case 2:
			SSetLightColor kModeMadness, kLightLoopLeft, color, NewState
		case 3:
			SSetLightColor kModeMadness, kLightOrbitLeft, color, NewState
		case 4:
			SSetLightColor kModeMadness, kLightRampCenter, color, NewState
		case 5:
			SSetLightColor kModeMadness, kLightLoopRight, color, NewState
		case 6:
			SSetLightColor kModeMadness, kLightRampRight, color, NewState
		case 7:
			SSetLightColor kModeMadness, kLightOrbitRight, color, NewState
	End select 
End Sub 

Function GetMadnessIdx(lIndex)
	GetMadnessIdx=-1
	Select case lIndex 
		case kLightSpinnerLeft:GetMadnessIdx=0
		case kLightRampLeft:GetMadnessIdx=1
		case kLightLoopLeft:GetMadnessIdx=2
		case kLightOrbitLeft:GetMadnessIdx=3
		case kLightRampCenter:GetMadnessIdx=4
		case kLightLoopRight:GetMadnessIdx=5
		case kLightRampRight:GetMadnessIdx=6
		case kLightOrbitRight:GetMadnessIdx=7
	End select 
End Function 

' https://youtu.be/Gz0v5fx0nqw?t=721
Dim MadnessShots(8)														' Which Madness shots are lit
Dim MadnessShotCntInit				' Total Red Shots
Dim MadnessShotCnt					' Total Red Shots Remaining
Dim MadnessJPCnt
Sub ProcessMadness(lIndex)
	dim PlayScene		' 0=Shot Locked, 1=Madness JP
	dim bUnderworld:bUnderworld=False 
	Dim i
	dim SaveCnt			' How many Red are there 
	dim curIdx
	dim SavePos			' Hold the first Empty slot
	Dim RotateDir
	Dim RotatedArray(8)
	if isModeActive(kModeCyborg) then Exit Sub 							' Cant stack with Cyborg
	if isModeActive(kModeMadness)=False then exit sub
WriteToLog "     ", "ProcessMadness: " & lIndex & " "

	If lIndex=kLightSpinnerLeft and Spinner001.TimerEnabled then exit sub   ' Wait until it stop spinning before we count this again 

	if lIndex=LeftFlipperKey or lIndex=RightFlipperKey Then 	' Rotate left or right, skipping over the non flashing shots
		RotateDir=-1
		if lIndex=LeftFlipperKey then RotateDir=1

		SaveCnt=8-MadnessShotCnt
		SavePos=-1
		For i = 0 to 7
			if MadnessShots(i)<=0 then 		' found free one
				SavePos=i
				exit For 
			End if
		Next
WriteToLog "     ", "SavePos:" & SavePos & " " & SaveCnt

		if SavePos<>-1 and SaveCnt>0 then		' If there is at least one empty shot and one red shot then shift 
			curIdx=SavePos
			For i = 0 to 6		' Shift all skipping over the green ones
				curIdx=curIdx+RotateDir
				if curIdx=8 then curIdx=0 
				if curIdx<0 then curIdx=7 
WriteToLog "     ", "CurIdx:" & curIdx & " SavePos:" & SavePos & " MadnessShots(curIdx):" & MadnessShots(curIdx) & " " & MadnessShots(SavePos)

				if MadnessShots(curIdx)=2 then		' Flashing red 
					MadnessShots(SavePos)=MadnessShots(curIdx)
					MadnessShots(curIdx)=0
					SavePos=curIdx
				elseif MadnessShots(curIdx)<=0 then	' Save the last free one
					SavePos=curIdx
				End if 
			Next 
			SetMadnessLightAll
		End if 
 
	elseif lIndex=kLightUnderworld and GetLightState(kModeMadness , kLightRampCenter)=0 then 	' Invalid - just eject
		PlaySoundVol "sfx_madnesHit2", VolSfx
		vpmtimer.addtimer 4000, "Exit_Underworld True '"

	Else 
WriteToLog "     ", "ProcessMadness: " & lIndex & " " & GetLightState(kModeMadness , lIndex) & " " & GetLightColor(kModeMadness, lIndex)

		if lIndex=kLightUnderworld then bUnderworld=True:lIndex=kLightRampCenter		' Handle Underworld Lock
		if GetLightState(kModeMadness , lIndex)<>0 then
			
			if GetLightColor(kModeMadness, lIndex)=red Then	' Hit a red shot, turn it green and see if we ge the JP
				PlayScene=0
				MadnessShots(GetMadnessIdx(lIndex))=3
				SSetLightColor kModeMadness, lIndex, green, 1

				SetLightColor lRampLeft, green, 2
				SetLightColor lRampRight, green, 2
				SetLightColor lRampCenter, green, 2
				SetLightColor lLoopLeft, green, 2
				SetLightColor lLoopRight, green, 2
				SetLightColor lOrbitLeft, green, 2
				SetLightColor lOrbitRight, green, 2
				SetLightColor lSpinnerLeft, green, 2
				FlashBeacon "green", True
				vpmTimer.AddTimer 1000, "ProcessMadness_FlashStop " & lIndex & " '"		' Turn it off after 1 seconds 
				MadnessShotCnt=MadnessShotCnt-1

WriteToLog "     ", "bUnderworld:" & bUnderworld & " " & MadnessShotCnt
				if bUnderworld then 	' Handle Underworld Lock
					if MadnessShotCnt<>0 then 			' This is the last shot we dont lock a ball
						RampDown						' Close it
						RemoveHudInfo kModeMadness
						AddHudInfo kModeMadness, "MADNESS", "LOCK", "", ":15", False 
						AddHudInfo kModeMadness, "CAN I PLAY", "WITH MADNESS", "", "", True  
						tmrMadnessLock.UserValue=15
						tmrMadnessLock.Interval=1000
						tmrMadnessLock.Enabled=True
					End if 
				End if 

				if MadnessShotCnt=0 then ' No more red shots, SJP
					PlayScene=1				' Skip the Shot Locked! Scene
					MadnessJPCnt=MadnessJPCnt+1

					PlaySoundVol "sfx_madnesHit3", VolSfx
					' Show Score 
					AddJPMode kModeMadness, 5000000*MadnessJPCnt
					AddScoreMode kModeMadness, ModeJPPoints(CurrentPlayer, kModeMadness)
					QueueScene "ProcessMadness_Jackpot " & ModeJPPoints(CurrentPlayer, kModeMadness) & " ", 2000, 1
					QueueScene "SceneClearLabels ", 0, 1
'					AddMultiball 1							' Add a Ball

					' Handle Underworld Lock (Premium)
					if tmrMadnessLock.Enabled then 		' We are coundown down lock
						RampUp 
						TriggerScript 400, "Exit_Underworld True"
						RemoveHudInfo kModeMadness  	' Go back to single HUD Info without Timer 
						AddHudInfo kModeMadness, "CAN I PLAY", "WITH MADNESS", "", "", True 
						tmrMadnessLock.Enabled = False
					else 
						RampUp 
						TriggerScript 400, "Exit_Underworld True"
					End if 
					' End Handle Underworld Lock

					' Add another red insert if we can
					MadnessShotCntInit=MadnessShotCntInit+1
					MadnessShotCnt=MadnessShotCntInit
					if MadnessShotCntInit <> 8 then
						For i = 0 to 7									' Turn all the Green Inserts back to red 
							if MadnessShots(i)=3 then MadnessShots(i)=2 
						Next

						i = INT(RND * 8)
						while MadnessShots(i)<>0 
							i = INT(RND * 8)
						Wend
						MadnessShots(i)=2
					else  ' Reset shots
						MadnessShotCnt=MadnessShotCntInit
						For i = 0 to 7		' Turn them all red again
							MadnessShots(i)=2 
						Next
					End if 
					SetMadnessLightAll
				End if

				If PlayScene=0 then 
					PlaySoundVol "sfx_madnesHit2", VolSfx

					' Show Score 
					AddScoreMode kModeMadness, 666000 + 1000000*(MadnessShotCntInit-MadnessShotCnt)
					QueueScene "ProcessMadness_ShotLocked " & 666000 + 1000000*(MadnessShotCntInit-MadnessShotCnt) & " ", 2000, 1
					QueueScene "SceneClearLabels ", 0, 1
				End if 
			elseif GetLightColor(kModeMadness, lIndex)=green Then			' Hit a green shot, change red, start scoring over
				PlaySoundVol "sfx_madnesHit4", VolSfx
				MadnessShotCnt=MadnessShotCnt+1
				MadnessShots(GetMadnessIdx(lIndex))=2
				SSetLightColor kModeMadness, lIndex, red, 2
			End if 
		End if 
	End if 
End Sub 

Sub ProcessMadness_FlashStop(lIndex)
	FlashBeacon "white", False
	SetLightColorRestore kLightRampLeft	
	SetLightColorRestore kLightRampRight
	SetLightColorRestore kLightRampCenter
	SetLightColorRestore kLightLoopLeft	
	SetLightColorRestore kLightLoopRight	
	SetLightColorRestore kLightOrbitLeft
	SetLightColorRestore kLightOrbitRight
	SetLightColorRestore kLightSpinnerLeft	
End Sub 

Sub ProcessMadness_Jackpot(Score)		' Flash the score in red for 2seconds with Image 
	PuPlayer.LabelSet pDMDFull, "Msg2", FormatScore(Score),1,"{'mt':2,'color':"&pupColorRed&",'size':6,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 3, 'yoffset': 3,'outline':1}"
	PuPlayer.LabelSet pDMDFull, "Msg2", FormatScore(Score),1,"{'mt':1,'at':1,'fq':150,'len':2000}"		' Flash it
	SceneImage "WizMadness\\txtMadnessJP.png"	
End Sub 

Sub ProcessMadness_ShotLocked(Score)
	PuPlayer.LabelSet pDMDFull, "Msg2", FormatScore(Score),1,"{'mt':2,'color':"&pupColorRed&",'size':6,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 3, 'yoffset': 3,'outline':1}"
	PuPlayer.LabelSet pDMDFull, "Msg2", FormatScore(Score),1,"{'mt':1,'at':1,'fq':150,'len':2000}"		' Flash it
	SceneImage "WizMadness\\txtMadnessLocked.png"	
End Sub 

Sub tmrMadnessLock_Timer()
	tmrMadnessLock.UserValue=tmrMadnessLock.UserValue-1

	if tmrMadnessLock.UserValue>=0 then 
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & tmrMadnessLock.UserValue,1,""  
		UpdateClock tmrMadnessLock.UserValue
	End if 

	if tmrMadnessLock.UserValue=-2 then 

		MadnessShots(GetMadnessIdx(kLightRampCenter))=3
		SetMadnessLight GetMadnessIdx(kLightRampCenter), 3
		Exit_Underworld False

		RemoveHudInfo kModeMadness  	' Go back to single HUD Info without Timer 
		AddHudInfo kModeMadness, "CAN I PLAY", "WITH MADNESS", "", "", True 

		tmrMadnessLock.Enabled = False
	End if 
End Sub 


Dim TrooperProgress:TrooperProgress=0
Sub ProcessTrooper(lIndex)
	dim JPtxt
	Dim bAllWhite:bAllWhite=False 
	dim AwardEddie:AwardEddie=0 
	Dim idx
	Dim bCheckSJP:bCheckSJP=False 
	Dim CannonScore:CannonScore=0
	Dim i
	dim j
	dim SpottedShots:SpottedShots=0

	If IsModeActive(kModeNOTB) then Exit sub							' Cant stack with NOTB 
	if isModeActive(kModeRTTH) then Exit Sub 							' Cant stack with RTTH
	if isModeActive(kModeCyborg) then Exit Sub 							' Cant stack with Cyborg
	if IsModeActive(kModeRime) then Exit Sub							' Cant stack with Rime Multiball 
	if IsModeActive(kModeAces) then Exit Sub							' Cant stack with Aces High 
	if IsModeQual(kModeMummy) then Exit Sub								' Cant stack with Mummy
	if IsModeActive(kMode2M2M) then Exit Sub							' Cant stack with 2M2M
	if isModeActive(kModeTrooper)=False then exit sub		' Trooper not active 

WriteToLog "     ", "ProcessTrooper:" & lIndex

	if isModeQual(kModeTrooper)=False then 								' Trooper Not Qualified yet
		if lIndex=kLightOrbitLeft2 or lIndex=kLightOrbitRight2 then 
WriteToLog "     ", "ProcessTrooper: Qual" & GetLightState(kModeTrooper, lIndex)
			if lIndex=kLightOrbitLeft2 then lIndex=kLightOrbitLeft
			if lIndex=kLightOrbitRight2 then lIndex=kLightOrbitRight

			if GetLightState(kModeTrooper, lIndex)<>0 then
				post001_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall
				post002_IsDropped(0)':PlaySoundAt "fx_SolenoidOn", gion_bulbs_backwall

				TrooperLocks(CurrentPlayer)=TrooperLocks(CurrentPlayer)+1
				LightEffect 1
				DMD CL(0, "TROOPER"), CL(1, "BALL " & TrooperLocks(CurrentPlayer) & " LOCKED"), "", eNone, eBlink, eNone, 1500, True, ""

				if TrooperLocks(CurrentPlayer) < 3 then 
					PlaysoundVol "vo_trooperlock" & TrooperLocks(CurrentPlayer), VolDef
					QueueFlush 0
					QueueScene2 0,"SceneGeneralStartDef False, False, ""TrooperMultiball"", ""Lock" & TrooperLocks(CurrentPlayer) & ".mp4"", ""^^^^^^^^^"" ", 2002, 1, True
					QueueScene2 0,"SceneClearLabels", 0, 1, True 
					QueueScene2 0,"QueueStartDefault", 2000, 1, True
					QueueScene2 0,"post001_IsDropped(1):post002_IsDropped(1):PlaySoundAt ""fx_SolenoidOn"", gion_bulbs_backwall", 0, 1, True 
					if TrooperLocks(CurrentPlayer)>=1 then Light007.state = 1
					if TrooperLocks(CurrentPlayer)=2 then Light006.state = 1
					PauseTimers(6000)

					if TrooperMBStartedCount(CurrentPlayer)>0 then 
						SetModeActive kModeTrooper, False
						SSetLightColor kModeTrooper, kLightOrbitLeft, green, 0
						SSetLightColor kModeTrooper, kLightOrbitRight, green, 0
					End if 
				Else  ' Start TrooperMB
					QueueFlush 0
					QueueSkipNoDef 0		' Cancel currently running clip
					QueueScene2 0,"SceneGeneralStartDef False, False, ""TrooperMultiball"", ""Lock" & TrooperLocks(CurrentPlayer) & ".mp4"", ""^^^^^^^^^"" ", 2000, 1, True
					QueueScene2 0,"PlaySoundVol ""vo_ball" & TrooperLocks(CurrentPlayer) & "locked"", VolDef", 4000, 1, True
					QueueScene2 0,"AddMultiball 2", 1200, 1, True 
					QueueScene2 0,"PlaySoundVol ""vo_troopermultiball"", VolDef", 3510, 1, True
					QueueScene2 0,"SceneClearLabels", 0, 1, True
					QueueScene2 0,"QueueStartDefault", 2000, 1, True
					QueueScene2 0,"post001_IsDropped(1):post002_IsDropped(1):PlaySoundAt ""fx_SolenoidOn"", gion_bulbs_backwall", 0, 1, True 
					QueueScene2 0,"SetModeQual kModeTrooper, True", 0, 1, True 	' Dont allow AddMultiball to complete shots
					PauseTimersForce(13000)   ' Force the timer since we are in MB

					StartTrooperMB
				End if 
			End if 
		End if 
	Else 	' We are qualified and active 

		if lIndex=kLightSpinnerLeft or lIndex=kLightSpinnerRight then
			ModeJPPoints(CurrentPlayer, kModeTrooper)=ModeJPPoints(CurrentPlayer, kModeTrooper)+5000
			AddScoreMode kModeTrooper, 25000
			PuPlayer.LabelSet pDMDFull,"Msg3",FormatScore(ModeJPPoints(CurrentPlayer, kModeTrooper)),1,""
		End if 

		if lIndex=kLightBumper then
			ModeJPPoints(CurrentPlayer, kModeTrooper)=ModeJPPoints(CurrentPlayer, kModeTrooper)+30000
			AddScoreMode kModeTrooper, 100000
			PuPlayer.LabelSet pDMDFull,"Msg3",FormatScore(ModeJPPoints(CurrentPlayer, kModeTrooper)),1,""

		elseif lIndex=kLightSJP then 						' Super Jackpot
			if GetLightState(kModeTrooper, lIndex)<>0 then
				AddScoreMode kModeTrooper, ModeSJPPoints(CurrentPlayer, kModeTrooper)

				StartTombTreasure(kTombTrooperSJP)
				bTrooperCannonReady=True
				TrooperProgress=0
				SSetLightColor kModeTrooper, kLightSJP, red, 0
				SSetLightColor kModeTrooper, kLightTargetX4, red, 0	
				AwardEddie=1
				bAllWhite=True 
				' Relight Inserts based on new Multiplier values 
				For i = 0 to 7
					if i<>kLightRampCenter then 
						Select case TrooperMBMultiplier(CurrentPlayer, i)
							Case 1:SSetLightColor kModeTrooper, i, Blue, 2:bAllWhite=False 
							Case 2:SSetLightColor kModeTrooper, i, Green, 2:bAllWhite=False 
							Case 3:SSetLightColor kModeTrooper, i, White, 2
							Case 4:SSetLightColor kModeTrooper, i, Orange, 2
							Case 5:SSetLightColor kModeTrooper, i, Red, 2
						End Select
					End if
				Next 

				PlaySoundVol "vo_superjackpot", VolDef
				JPtxt=FormatScore(ModeSJPPoints(CurrentPlayer, kModeTrooper))
				QueueFlush 0
				QueueScene "PlaySoundVol ""sfx_trooperSJP"", VolSfx", 0, 1
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""TrooperMultiball"", ""SJP.mp4"", ""I:TrooperMultiball\\txtSJP.png^^^^^^" & JPtxt & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 7000, 1
				QueueScene "SceneClearLabels", 0, 1

				if bAllWhite then AwardEddie=2		' If all shots are 3x (white) or greater award eddie card 2 
				AddEddieCard kModeTrooper, AwardEddie=2
				ModeSJPPoints(CurrentPlayer, kModeTrooper)=0

			End if 
		elseif lIndex=kLightRampCenter then 					' Cannon Shot
			if GetLightState(kModeTrooper, lIndex)<>0 then 		' RainbowLight
				For i = 1 to BullseyeMultiplier					' Find shots to spot 
					For j = 0 to 7								
						if GetLightState(kModeTrooper, i)<>0 then 
							AddScoreMode kModeTrooper, ModeJPPoints(CurrentPlayer, kModeTrooper)
							ModeJPPoints(CurrentPlayer, kModeTrooper)=ModeJPPoints(CurrentPlayer, kModeTrooper)+(15000*TrooperMBMultiplier(CurrentPlayer, i))

							if TrooperMBMultiplier(CurrentPlayer, i)=5 then 		' Hit any 5x Shot awards level 2
								AddEddieCard kModeTrooper, True
							End if 
							if TrooperMBMultiplier(CurrentPlayer, i)<5 then 		' Max is 5x
								TrooperMBMultiplier(CurrentPlayer, i)=TrooperMBMultiplier(CurrentPlayer, i)+1
							End if 

							SpottedShots=SpottedShots+1
							CannonScore=CannonScore+ModeJPPoints(CurrentPlayer, kModeTrooper)

							ModeSJPPoints(CurrentPlayer, kModeTrooper)=ModeSJPPoints(CurrentPlayer, kModeTrooper)+ModeJPPoints(CurrentPlayer, kModeTrooper)
							SSetLightColor kModeTrooper, i, blue, 0
							TrooperProgress=TrooperProgress+1
						End if 
					Next 
				Next 

				if bTrooperMBFirstHit then 		' You get 1 MB-Add during trooper 
					bTrooperMBFirstHit=False 
					AddMultiball 1
				End if 

				bTrooperCannonReady=False
				StopRainbow
				lUnderworld.State = 0
				SetDefPulse lUnderworld
				PharaohBullseyeFlasherEnabled False

				JPtxt=FormatScore(CannonScore)
				QueueFlush 0
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""TrooperMultiball"", ""Cannon.mp4"", ""I:TrooperMultiball\\txtCannon"& BullseyeMultiplier &".png^^^^^^" & JPtxt & "^+" & SpottedShots  & " Shots Spotted^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3600, 1
				QueueScene "SceneClearLabels", 0, 1

				bCheckSJP=True
			End if 
		elseif GetLightState(kModeTrooper, lIndex)<>0 and lIndex<=kLightSpinnerLeft then

			Select case TrooperMBMultiplier(CurrentPlayer, lIndex)
				case 1:
					PlaySoundVol "vo_JP" & INT(RND*2)+1, VolDef
				case 2:
					PlaySoundVol "vo_doublejackpot", VolDef
				case 3:
					PlaySoundVol "vo_triplejackpot", VolDef
				case 4: 
					PlaySoundVol "vo_JP" & INT(RND*2)+1, VolDef
				case 5:
					PlaySoundVol "vo_JP" & INT(RND*2)+1, VolDef
			End Select 
			AddScoreMode kModeTrooper, ModeJPPoints(CurrentPlayer, kModeTrooper)+100000		' is the 100K base value???
			ModeJPPoints(CurrentPlayer, kModeTrooper)=ModeJPPoints(CurrentPlayer, kModeTrooper)+(15000*TrooperMBMultiplier(CurrentPlayer, lIndex))

			ModeSJPPoints(CurrentPlayer, kModeTrooper)=ModeSJPPoints(CurrentPlayer, kModeTrooper)+ModeJPPoints(CurrentPlayer, kModeTrooper)
			SSetLightColor kModeTrooper, lIndex, blue, 0
			idx=INT(RND * 5)+1
			TrooperProgress=TrooperProgress+1
			JPtxt=FormatScore(ModeJPPoints(CurrentPlayer, kModeTrooper))
			bCheckSJP=True 

			QueueFlush 0
			QueueScene "SceneGeneralStart pDMDFull, False, False, ""TrooperMultiball"", ""Award"&idx&".mp4"", ""I:TrooperMultiball\\txtJP"& TrooperMBMultiplier(CurrentPlayer, lIndex) &".png^^^^^^" & JPtxt & "^^^"", ""^^^^^^1500:" & pupColorRed & "^^^"" ", 1534, 1
			QueueScene "SceneClearLabels", 0, 1

			if TrooperMBMultiplier(CurrentPlayer, lIndex)=5 then 		' Hit any 5x Shot awards level 2
				AddEddieCard kModeTrooper, True
			End if 
			if TrooperMBMultiplier(CurrentPlayer, lIndex)<5 then 		' Max is 5x
				TrooperMBMultiplier(CurrentPlayer, lIndex)=TrooperMBMultiplier(CurrentPlayer, lIndex)+1
			End if 

'	SceneGeneralStart pDMDFull, True, False, "TrooperMultiball", "Wait1.mp4", "I:TrooperMultiball\\txtWait1.png^^6:" & FormatScore(ModeJPPoints(CurrentPlayer, kModeTrooper)) & "^^6:" & FormatScore(ModePoints(CurrentPlayer, kModeTrooper))&"^^^^^", "^^" & pupColorRed & "^^" & pupColorRed & "^^^^^"

		End if 
	End if 

	if bCheckSJP then 	' See if we light SJP
		if TrooperProgress>=3 then
			if GetLightState(kModeTrooper, kLightSJP)=0 then 
				Vpmtimer.AddTimer 1000, "PlaySoundVol ""vo_GetSJP"", VolDef '"
				SSetLightColor kModeTrooper, kLightSJP, red, 1
				SSetLightColor kModeTrooper, kLightTargetX4, red, 2
			End if 
		End if 

		if TrooperProgress>=6 then 	' Relight everything
			TrooperProgress=0
			For i = 0 to 7
				if i<>kLightRampCenter then 
					Select case TrooperMBMultiplier(CurrentPlayer, i)
						Case 1:SSetLightColor kModeTrooper, i, Blue, 2
						Case 2:SSetLightColor kModeTrooper, i, Green, 2
						Case 3:SSetLightColor kModeTrooper, i, White, 2
						Case 4:SSetLightColor kModeTrooper, i, Orange, 2
						Case 5:SSetLightColor kModeTrooper, i, Red, 2
					End Select
				End if 
			Next 
		End if
	End if

End Sub 


Sub ProcessMummy(lIndex)
	dim bAddBall:bAddBall=False
	Dim ScepterScore
	Dim ScepterJP
	Dim SJP0
	Dim SJP1
	Dim SJP2
	dim bSetColor:bSetColor=True 

	if bMummyDisabled then Exit Sub 									' Some other mode disabled mummy
	If IsModeQual(kModeTrooper) then Exit sub							' Cant stack with Trooper 
	If IsModeActive(kModeNOTB) then Exit sub							' Cant stack with NOTB 
	if isModeActive(kModeRTTH) then Exit Sub 							' Cant stack with RTTH
	if isModeActive(kModeCyborg) then Exit Sub 							' Cant stack with Cyborg
	if IsModeActive(kModeRime) then Exit Sub							' Cant stack with Rime Multiball 
	if IsModeActive(kModeAces) then Exit Sub							' Cant stack with Aces High 
	if IsModeActive(kMode2M2M) then Exit Sub							' Cant stack with 2M2M

WriteToLog "     ", "ProcessMummy:" & lIndex & " " & MummyCount(CurrentPlayer) & " " & bMummyLetterJP & " BallsInLock(CurrentPlayer):" & BallsInLock(CurrentPlayer)

	if ModeWaitPlayfieldQual(CurrentPlayer, kModeMummy) then			' We Paused Timers until table is qualified
		ModeWaitPlayfieldQual(CurrentPlayer, kModeMummy)=False
		PauseTimersForce 2000
	End if

	if IsModeQual(kModeMummy)=False then 								' Mummy Not Started Yet 
		if lIndex=kLightCaptiveBall then 
			if BallsInLock(CurrentPlayer)=0 and MummyCount(CurrentPlayer) <5 then	' Step 1: Spell Mummy (No Balls Locked)

				MummyTogglePos(CurrentPlayer)=Not MummyTogglePos(CurrentPlayer)
				If (MummyStartCnt(CurrentPlayer)<=1) or (MummyStartCnt(CurrentPlayer)>1 and MummyTogglePos(CurrentPlayer))  then 	' 2 hits when greater than 2

					AddScoreMode kModeMummy, 10000
					AddMummyletter
				End if 
			Elseif BallsInLock(CurrentPlayer)>0 then 								' Step 2: Spell Mummy again and knock it out to Start MummyMB

				if MummyCount(CurrentPlayer)>6 then 					' ERROR TRAP!!!
WriteToLog "     ", "ERROR DETECTED - MAKE A NOTE!!"
					PlaySoundVol "alarm", VolSfx
					BallsInLock(CurrentPlayer)=0
					MummyCount(CurrentPlayer)=3
				End if 

				AddScoreMode kModeMummy, 10000
				AddMummyletter
			End if
		End if 
	elseif lIndex=kLightCaptiveBall then								' Step 4: Open Ramp to Lock ball again
		if bMummyLetterJP=True then 
WriteToLog "     ", "bMummySJPCnt:" & bMummySJPCnt(CurrentPlayer)
			if bMummySJPCnt(CurrentPlayer)>=1 then 
WriteToLog "     ", "CycleMummyInsertsStates:" & tmrMummyCycleState.UserValue-1 & " " & CycleMummyInsertsStates(tmrMummyCycleState.UserValue-1)
				if CycleMummyInsertsStates(tmrMummyCycleState.UserValue-1)=0 then 		' Doesnt count if it is already lit
					AddScoreMode kModeMummy, 10000
					AddMummyletter
					SSetLightColor kModeMummy, kLightCaptiveBall, red, 0
					SetDefPulse(lMummyM)
					SetDefPulse(lMummyU)
					SetDefPulse(lMummyM2)
					SetDefPulse(lMummyM3)
					SetDefPulse(lMummyY)
					CycleMummyInsertsStates(tmrMummyCycleState.UserValue-1)=1
					CycleMummyInsertsState(False)
				End if 
			Else 
				AddScoreMode kModeMummy, 10000
				AddMummyletter											' Play Collect JP Animation
				SSetLightColor kModeMummy, kLightCaptiveBall, red, 0
				SetDefPulse(lMummyM)
				SetDefPulse(lMummyU)
				SetDefPulse(lMummyM2)
				SetDefPulse(lMummyM3)
				SetDefPulse(lMummyY)
				Select Case MummyCount(CurrentPlayer)
					Case 0:lMummyM.State = 0
					Case 1:lMummyU.State = 0
					Case 2:lMummyM2.State= 0
					Case 3:lMummyM3.State= 0
					Case 4:lMummyY.State = 0
				End Select 
			End if 
		End if 

	' Step 3: Switch Hits to Relight Lock
	else

		' All Switch Hits progress lighting Jackpot
		if lIndex=kLightSwitch or lIndex=kLightTargetX1 or lIndex=kLightTargetX2 or lIndex=kLightTargetX3 or lIndex=kLightTargetX4 _
			or lIndex=kLightSpinnerLeft or lIndex=kLightSpinnerRight or lIndex=kLightSpinnerStop _
			or lIndex=kLightSlingLeft or lIndex=kLightSlingRight or GetLightState(kModeMummy, lIndex)<>0 then

			if IsModeActive(kModeMummy) and MummyTimes(CurrentPlayer)>=1 then 

				if lIndex = kLightSpinnerStop then 
					SceneMummyWait
				elseif bMummyLetterJP or bMummySJP then 

					if lIndex <> kLightSpinnerStop then 
						if MummySwitchHitsDoubled(CurrentPlayer) then 
							AddJPMode kModeMummy, 15000*2:AddScoreMode kModeMummy, 15000*2
						Else 
							AddJPMode kModeMummy, 15000:AddScoreMode kModeMummy, 15000
						End if
					End if 

					if lIndex<>kLightSpinnerLeft and lIndex<>kLightSpinnerRight then	' Dont update
						SceneMummyWait
					End if 			 
				Else
					MummySwitchHits(CurrentPlayer)=MummySwitchHits(CurrentPlayer)+1
					if MummySwitchHitsDoubled(CurrentPlayer) then 
						AddJPMode kModeMummy, 15000*2:AddScoreMode kModeMummy, 15000*2
					Else 
						AddJPMode kModeMummy, 15000:AddScoreMode kModeMummy, 15000
					End if 

					If MummySwitchHits(CurrentPlayer) = MummySwitchHitsMax(CurrentPlayer) then	' Light Mummy Letter JP 
						if INT(RND*2)=0 then 
							PlaySoundVol "vo_shootCaptiveBall", VolDef
						else 
							PlaySoundVol "vo_JPisLit", VolDef
						End if 
						bMummyLetterJP=True
						SSetLightColor kModeMummy, kLightCaptiveBall, red, 2
						SetFastPulse lCaptiveBall
						SetFastPulse(lMummyM)
						SetFastPulse(lMummyU)
						SetFastPulse(lMummyM2)
						SetFastPulse(lMummyM3)
						SetFastPulse(lMummyY)
						if bMummySJPCnt(CurrentPlayer)>=1 then CycleMummyInsertsState(True)
						UpdateMummy2
						'MummyCount(CurrentPlayer)=0
					End if 

					if lIndex=kLightSpinnerLeft or lIndex=kLightSpinnerRight then		' Just update text so screen doesnt flicker
						PuPlayer.LabelSet pDMDFull,"Msg10", MummySwitchHits(CurrentPlayer) & "/" & MummySwitchHitsMax(CurrentPlayer), 1,""
					else 
						SceneMummyWait
					End if 			
				End if 

			End if 
		End if 

		' Lit Shots progress - Add-A-Ball
		if GetLightState(kModeMummy, lIndex)<>0 and lIndex<=kLightOrbitRight then ' Process ramps Only
WriteToLog "     ", "ProcessMummy lIndex:" & lIndex

			if bMummySJP and lIndex=kLightRampCenter then 					' SUPER JACKPOT !!!
				bMummySJPCnt(CurrentPlayer)=bMummySJPCnt(CurrentPlayer)+1
				bMummySJP=False 
				StartTombTreasure(kTombMummySJP)
				AddEddieCard kModeMummy, bMummySJPCnt(CurrentPlayer)=2		'2nd SJP Awards Eddie Card2
				
				if BullseyeMultiplier=1 then SJP0="":PlaySoundVol "vo_superjackpot", VolDef
				if BullseyeMultiplier=2 then SJP0="DOUBLE":PlaySoundVol "vo_superduperjackpot", VolDef
				if BullseyeMultiplier=3 then SJP0="TRIPLE":PlaySoundVol "vo_superjackpot3", VolDef

				AddScore ModeJPPoints(CurrentPlayer, kModeMummy) * 2 * BullseyeMultiplier
				SJP1=FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy) * 2)
				SJP2=FormatScore(ModeJPPoints(CurrentPlayer, kModeMummy) * BullseyeMultiplier)
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""MummyMultiball"", ""MSuperJP.mp4"", ""^^"& SJP0 & "^^SUPER JACKPOT^^" & SJP1 & " X " & BullseyeMultiplier &  "^^" & SJP2 & "^"", ""^^^^^^^^3000:" & pupColorRed & "^"" ", 3000, 1
				QueueScene "SceneClearLabels", 0, 1

				MummyCount(CurrentPlayer)=0
				SetDefPulse(lMummyM)
				SetDefPulse(lMummyU)
				SetDefPulse(lMummyM2)
				SetDefPulse(lMummyM3)
				SetDefPulse(lMummyY)
				lMummyM.State = 0
				lMummyU.State = 0
				lMummyM2.State= 0
				lMummyM3.State= 0
				lMummyY.State = 0

				SSetLightColor kModeMummy, kLightRampCenter, red, 0
				lUnderworld.State = 0
				SetDefPulse lUnderworld
				PharaohBullseyeFlasherEnabled False

				MummySwitchHitsMax(CurrentPlayer)=10
				MummySwitchHits(CurrentPlayer)=0
			End if 
		
			SSetLightColor kModeMummy, lIndex, yellow, 0
			MummyScepterCnt(CurrentPlayer)=MummyScepterCnt(CurrentPlayer)+1
			ScepterScore=125000 + (25000*MummyScepterCnt(CurrentPlayer))

			If MummyAddBallMode(CurrentPlayer)=0 and _
				GetLightState(kModeMummy, kLightRampLeft)=0 and GetLightState(kModeMummy, kLightRampRight)=0 then 
				MummyAddBallMode(CurrentPlayer)=1

				SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
				SSetLightColor kModeMummy, kLightRampRight, yellow, 2
				SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
				SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2

				bAddBall=True 
			ElseIf MummyAddBallMode(CurrentPlayer)=1 and _
				GetLightState(kModeMummy, kLightRampLeft)=0 and GetLightState(kModeMummy, kLightRampRight)=0 and _
				GetLightState(kModeMummy, kLightOrbitLeft)=0 and GetLightState(kModeMummy, kLightOrbitRight)=0 then 
				MummyAddBallMode(CurrentPlayer)=2

				SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
				SSetLightColor kModeMummy, kLightRampRight, yellow, 2
				SSetLightColor kModeMummy, kLightRampCenter, yellow, 2
				SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 2
				SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 2
				SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
				SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2

				bAddBall=True 

			ElseIf MummyAddBallMode(CurrentPlayer)=2 and GetLightState(kModeMummy, kLightRampCenter)=0 and _
				GetLightState(kModeMummy, kLightRampLeft)=0 and GetLightState(kModeMummy, kLightRampRight)=0 and _
				GetLightState(kModeMummy, kLightLoopLeft)=0 and GetLightState(kModeMummy, kLightLoopRight)=0 and _
				GetLightState(kModeMummy, kLightOrbitLeft)=0 and GetLightState(kModeMummy, kLightOrbitRight)=0 then 

				if MummyAddBallCnt(CurrentPlayer)<2 then 			' Only get 3 per mummyMB
					SSetLightColor kModeMummy, kLightRampLeft, 	yellow, 2
					SSetLightColor kModeMummy, kLightRampRight, yellow, 2
					SSetLightColor kModeMummy, kLightRampCenter, yellow, 2
					SSetLightColor kModeMummy, kLightLoopLeft, 	yellow, 2
					SSetLightColor kModeMummy, kLightLoopRight, 	yellow, 2
					SSetLightColor kModeMummy, kLightOrbitLeft, 	yellow, 2
					SSetLightColor kModeMummy, kLightOrbitRight, 	yellow, 2
				End if 

				bAddBall=True 
			End if 

			if bAddBall then 
				ScepterJP=1750000
				if MummyAddBallCnt(CurrentPlayer)=1 then ScepterJP=2500000
				if MummyAddBallCnt(CurrentPlayer)=2 then ScepterJP=4250000
				QueueScene "SceneGeneralStartDef False, False, ""MummyMultiball"", ""MummyMB.mp4"", ""I:MummyMultiball\\txtBallAdded.png^^^^^^^^^"" ", 1067, 1	
				QueueScene "SceneClearLabels", 0, 1
				EnableBallSaver BallSaverTime
				PlaySoundVol "vo_addaball", VolDef
				AddMultiball 1
				MummyAddBallCnt(CurrentPlayer)=MummyAddBallCnt(CurrentPlayer)+1

				QueueScene "SceneGeneralStart pDMDFull, False, False, ""MummyMultiball"", ""Scepter.mp4"", ""^^^^SCEPTER JACKPOT^^" & FormatScore(ScepterJP) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3000, 1
				QueueScene "SceneClearLabels", 0, 1
				AddScoreMode kModeMummy, ScepterJP
			else 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""MummyMultiball"", ""Scepter.mp4"", ""I:MummyMultiball\\Scepter.png^^^^^^" & FormatScore(ScepterScore) & "^^^"", ""^^^^^^3000:" & pupColorRed & "^^^"" ", 3000, 1		
				QueueScene "SceneClearLabels", 0, 1
			End if 
			AddScoreMode kModeMummy, ScepterScore

		End if 
	end if 

End Sub 


'***********************
'        REVIVE
'***********************
' REVIVE ball saver ' Now operating the same as STERN
' Shoot the left spinner to spell REVIVE. 
' Spelling REVIVE lights outlanes for a REVIVE outlane ball-save.
' The REVIVE icon in bottom-left corner of the screen shows your progress and letters/spins needed to enable REVIVE
' When you spell REVIVE, both outlanes will light for Revive. However, each outlane’s Revive is used individually.
' You cannot stack Revives. New REVIVE Letters cannot be spotted while both outlane Revives are lit, but the letters can be spotted if only one outlane is lit. 
' Lit Revives carry over between balls. If both Revives are lit, the spinner scoring is boosted.
' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=721
' https://youtu.be/KFrRH1r8iV4?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1710
' https://youtu.be/KFrRH1r8iV4?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=3542
Sub ProcessRevive(lIndex)
	if lIndex = kLightSpinnerLeft then 

		if REVIVELActive(CurrentPlayer)=False or REVIVERActive(CurrentPlayer)=False then  

WriteToLog "     ", "ProcessRevive:" & REVIVECountDown(CurrentPlayer) & " " & REVIVEProgress(CurrentPlayer)
			REVIVECountDown(CurrentPlayer)=REVIVECountDown(CurrentPlayer)-1

			if REVIVECountDown(CurrentPlayer)=0 then 
				REVIVEProgress(CurrentPlayer)=REVIVEProgress(CurrentPlayer)+1
				puPlayer.LabelSet pDMDFull,"pREVIVE","PuPOverlays\\REVIVE_" & REVIVEProgress(CurrentPlayer) & ".gif",1,"{'mt':2,'color':0,'width':12, 'height':22, 'anigif':100, 'xpos':0.1, 'ypos':72,'zback':1}"

				if REVIVEProgress(CurrentPlayer)=6 then 
					
					PlaySoundVol "vo_Rev_" & REVIVEProgress(CurrentPlayer), VolDef
					QueueScene "PlaySoundVol ""sfx_Rev_6"", VolSfx", 1, 1
					QueueScene "SceneGeneralStartDef False, False, ""Revive"", ""REVIVE_" & REVIVEProgress(CurrentPlayer) & ".mp4"", ""^^^^^^10:750,000^^^"" ", 3866, 1
					QueueScene "PlaySoundVol ""vo_ReviveLit"", VolSfx", 1, 1
					QueueScene "SceneClearLabels", 0, 1
					AddScore 750000

					REVIVECount(CurrentPlayer)=REVIVECount(CurrentPlayer)+1
					REVIVEProgress(CurrentPlayer)=0
					VPMTimer.AddTimer 1000, "puPlayer.LabelSet pDMDFull,""pREVIVE"",""PuPOverlays\\REVIVE_6.gif"",1,""{'mt':2,'color':0,'width':12, 'height':22, 'anigif':100, 'xpos':0.1, 'ypos':72,'zback':1}"" '"
					light005.state = 0
					Light004.state =1
					Light003.state =1
					
					REVIVELActive(CurrentPlayer) = True 
					REVIVERActive(CurrentPlayer) = True
					DOF 172, DOFpulse 'Revive Effect
					
				else 
					PlaySoundVol "vo_Rev_" & REVIVEProgress(CurrentPlayer), VolDef
					QueueScene "SceneGeneralStartDef False, False, ""Revive"", ""REVIVE_" & REVIVEProgress(CurrentPlayer) & ".mp4"", ""^^^^^^10:750,000^^^"" ", 2033, 1
					QueueScene "SceneClearLabels", 0, 1
					AddScore 750000
				End if 

				if REVIVECount(CurrentPlayer)=0 then 
					REVIVECountDown(CurrentPlayer)=10
				else 
					REVIVECountDown(CurrentPlayer)=15
				End if 
			End if 
		End if
 
		if REVIVELActive(CurrentPlayer)=False or REVIVERActive(CurrentPlayer)=False  then 
			puPlayer.LabelSet pDMDFull,"pREVIVESPINS",REVIVECountDown(CurrentPlayer),1,""
		else 
			puPlayer.LabelSet pDMDFull,"pREVIVESPINS"," ",1,""
		End if 
		
	End if 
End Sub 


' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=597
' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1026
Dim LoopJPTotal
Sub ProcessLoops(lIndex)
	Dim Score
	Dim ScoreMult:ScoreMult=1
	Dim bLoop3x:bLoop3x=False
	dim bAddLoop:bAddLoop=False 
	Dim ClipIdx:ClipIdx=1
WriteToLog "     ", "ProcessLoops:" & lIndex & " " & LoopCount

	if LoopJackpotMulti(CurrentPlayer) < GetLoopJPMax() then
		if lIndex=kLightLoopLeft2 or (lIndex=kLightLoopLeft and LoopJackpotMulti(CurrentPlayer) = GetLoopJPMax()-1) then	' Count full loops except the last one
			bAddLoop=True
		elseif lIndex = kLightLoopRight2 then 	
			Loop2x=True
			bLoop3x=True
			bAddLoop=True
		End if 

		if LeftFlipper1.currentangle=LeftFlipper1.startangle then 	' 2x For Shooting with Flipper down
			Loop2x=True 
		Else 
			Loop2x=False 
		End if 

		if bAddLoop then 
			LoopExtraBallCount(CurrentPlayer) = LoopExtraBallCount(CurrentPlayer) + 1
			If LoopCount < 6 then LoopCount = LoopCount + 1
			Score=LoopCount*250000
			if bLoop3x then
				ScoreMult=3			'3x Score
			elseif Loop2x Then
				ScoreMult=2			'2x Score	
			End if

			If LoopExtraBallCount(CurrentPlayer) >= 30 then ' Light Extra Ball
				LightExtraBall()
			End If

			AddJP cLoopJackpotMulti, 1
			LoopJackpot(CurrentPlayer)=LoopJackpot(CurrentPlayer)+(score*ScoreMult)
			DMD CL(0, "LOOPS " & " X " & ScoreMult), "", "", eNone, eNone, eNone, 1000, True, ""
			pJackpotCounts False ' Update Jackpot and counter values HUD on the right

			if LoopJackpotMulti(CurrentPlayer) = GetLoopJPMax() then ' Light SJP
WriteToLog "     ", "LOOP-SJP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
				LoopJPTotal=0
				LoopJPCount=0
				LoopCount=6
				LoopTimer.interval = 60
				LoopTimer.Enabled = True
				Light021.state = 1
				Light022.state = 1
				if lIndex=kLightLoopLeft then 	' Qualifying shot on loop raises post
					post001_IsDropped(0) ' Raise the post 
					vpmtimer.AddTimer 2000, "WriteToLog ""     "", ""LOOP SJP-DONE!!!!!!!!!!!!!!!!!!!!!"":post001_IsDropped(1) '"
				End if 
				PlaySoundVol "vo_loopJPLit", VolDef
				QueueScene "SceneGeneralStartDef False, False, ""Callouts"", ""wickerman4.mp4"", ""^^LOOP JACKPOTS LIT^^" & FormatScore(LoopJackpot(CurrentPlayer)) & "^^^^^"" ", 2000, 1
				QueueScene "SceneClearLabels", 0, 1
				pJackpotCounts False
			else 
				if LoopCount>3 then ClipIdx=LoopCount-2
				AddScore score*ScoreMult
				PlaySoundVol "sfx_loop", VolSfx
				if ScoreMult=1 then 
					QueueScene "SceneGeneralStartDef False, False, ""Callouts"", ""wickerman"&ClipIdx&".mp4"", ""^^LOOP AWARD^^" & FormatScore(score) & "^^^^4.5:" & 6-LoopCount & " more to light Loop Jackpots. ^4.5:" & 30 - LoopExtraBallCount(CurrentPlayer) & " more to light Extra Ball"" ", 2000, 1
				Else 
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""wickerman"&ClipIdx&".mp4"", ""^^LOOP AWARD^^" & _
						FormatScore(score) & " X " & ScoreMult & "^^" & FormatScore(score*ScoreMult) & "^^4.5:" & 5-LoopCount & _
						" more to light Loop Jackpots. ^4.5:" & 30 - LoopExtraBallCount(CurrentPlayer) & " more to light Extra Ball"",""^^^^^^" & pupColorRed & "^^^"" ", 2000, 1
				End if 
				QueueScene "SceneClearLabels", 0, 1

				LoopTimer.Enabled = 0
				LoopTimer.UserValue=0
				UpdateLoopLights
				LoopTimer.Interval=1000
				LoopTimer.Enabled = 1
			End if
		End if 
	Elseif Light022.state <>0 and  Light021.state <> 0 then 		' SJP is lit 
		' Make sure you catch a partial loop if the post is up but full loop every other time
		if (lIndex=kLightLoopLeft2 and LoopJPCount<3) or (lIndex=kLightLoopLeft and LoopJPCount>=3) or lIndex = kLightLoopRight2 then
			LoopJPCount=LoopJPCount+1
			QueueFlush 0
			PlaySoundVol "sfx_loop", VolSfx
			if LoopJPCount*PlayfieldMultiplier(CurrentPlayer)<>1 then 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""wickerman5.mp4"", ""^^LOOP JACKPOT^^" & FormatScore(LoopJackpot(CurrentPlayer)) & " X " & (LoopJPCount*PlayfieldMultiplier(CurrentPlayer)) & "^^" & FormatScore((LoopJackpot(CurrentPlayer)*(LoopJPCount*PlayfieldMultiplier(CurrentPlayer)))) & "^^^"", ""^^^^^^2000:" & pupColorRed & "^^^"" ", 2000, 1
			Else 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Callouts"", ""wickerman5.mp4"", ""^^LOOP JACKPOT^^^^" & FormatScore((LoopJackpot(CurrentPlayer)*(LoopJPCount*PlayfieldMultiplier(CurrentPlayer)))) & "^^^"", ""^^^^^^2000:" & pupColorRed & "^^^"" ", 2000, 1
			End if 
'WriteToLog "     ", "LOOP JP:" & LoopJPCount
			LoopAnimate
			QueueScene "SceneClearLabels", 0, 1	
			StartTombTreasure(kTombLoopJP)
			Addscore LoopJackpot(CurrentPlayer)*LoopJPCount
			LoopJPTotal=LoopJPTotal+ (LoopJackpot(CurrentPlayer)*(LoopJPCount*PlayfieldMultiplier(CurrentPlayer)))
			if LoopJPCount>=4 then ' after 4th one it gets hard
				post001_IsDropped(0) ' Raise the post to catch last ball
				vpmtimer.AddTimer 2000, "post001_IsDropped(1) '"

'				LoopTimer.UserValue=11
'				LoopCount = 7
'				LoopTimer_Timer				' End Loop JP

			elseif LoopCount<>7 then 		' Start 12 second timer 
				LoopCount = 7
				Light021.state = 2
				Light022.state = 2
				LoopTimer.Enabled = 0
				LoopTimer.UserValue=0
				LoopTimer.Interval=1000
				LoopTimer.Enabled = True
			End if 

			pJackpotCounts True ' Update Jackpot and counter values on the right 
			UpdateLoopLights
		End if 
	End if 

End Sub 

Dim FearOfTheDarkSpinTotal
Sub ProcessEddie(lIndex)
	dim SaveIdx
	dim rndIdx
	dim tmpIdx
	dim i
	SaveIdx=lIndex
	if IsModeActive(kModeEddie) then 							  ' If a light for Eddie is lit we add progress
		if lIndex=kLightOrbitLeft then Exit Sub 				  ' Partial Left Orbit adds progress (Fake It)
		if lIndex=kLightOrbitLeft2 then SaveIdx=kLightOrbitLeft
WriteToLog "     ", "ProcessEddie:" & SaveIdx
		if GetLightState(kModeEddie, SaveIdx)<>0 then 

			if EddieRndIdx(CurrentPlayer, 0)<>-1 then 		' We are on phase 3 they go out and light another 

				SSetLightColor kModeEddie, SaveIdx, white, 0
				select case saveIdx
					case kLightRampLeft:   rndIdx=0
					case kLightOrbitLeft:  rndIdx=1
					case kLightLoopRight:  rndIdx=2
					case kLightRampRight:  rndIdx=3
					case kLightOrbitRight: rndIdx=4
				End Select
				for i = 0 to 2
					if EddieRndIdx(CurrentPlayer, i)=rndIdx then tmpIdx=i
				Next 

				rndIdx= INT(Rnd*5)
				while rndIdx = EddieRndIdx(CurrentPlayer, 0) or rndIdx=EddieRndIdx(CurrentPlayer, 1) or rndIdx=EddieRndIdx(CurrentPlayer, 2)
					rndIdx=Int(Rnd*5)
				wend
				EddieRndIdx(CurrentPlayer, tmpIdx)=rndIdx
				if rndIdx=0 then SSetLightColor kModeEddie,kLightRampLeft  , White, 1
				if rndIdx=1 then SSetLightColor kModeEddie,kLightOrbitLeft  , White, 1
				if rndIdx=2 then SSetLightColor kModeEddie,kLightLoopRight , White, 1
				if rndIdx=3 then SSetLightColor kModeEddie,kLightRampRight , White, 1
				if rndIdx=4 then SSetLightColor kModeEddie,kLightOrbitRight, White, 1

			End if 

			PlaySoundVol "sfx_gun15", VolSfx
			Addscore 25000
			AddEDDIELetter 1
			If EDDIELetter(CurrentPlayer)=5 then UpdateEDDIELetter		' Immediatly Update (TBD-NOTE: this can cause a glitch with some scenes because there is no text it clears the Image)
		End If
	End if 
End Sub 

' https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=32
' https://youtu.be/zaCEffQkj4o?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1452	
Sub ProcessFear(lIndex)
	if IsModeActive(kModeFear) then
WriteToLog "     ", "ProcessFear:" & lIndex & " " & FearOfTheDarkCount
		if lIndex=kLightSpinnerStop then 							' Spinner Stopped 
WriteToLog "     ", "kLightSpinnerStop:" & FearOfTheDarkSpinActive
			if FearOfTheDarkSpinActive then 
				FearOfTheDarkCount=FearOfTheDarkCount+1
				FearOfTheDarkSpinActive=False

				SceneFODWait_Spinners=False 
				QueueFlushForce 0, True 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""FearOfTheDark"", ""Spinner.mp4"", ""^^^^^^" & FormatScore(FearOfTheDarkSpinTotal) & "^^^"", ""^^^^^^1500:" & pupColorRed & "^^^"" ", 2533, 1
'				QueueScene "SceneClearLabels", 0, 1	
				AddScoreMode kModeFear, FearOfTheDarkSpinnerValue

				if FearOfTheDarkCount=3 then 						' Start Soul Shard (Target001_hit)
					FearOfTheDarkTimer.Enabled = False
					StartShardHurryup(kModeFear)
					SSetLightColor kModeFear, kLightRampCenter, white, 2
					SSetLightColor kModeFear, kLightRampLeft, 	purple, 0
					SSetLightColor kModeFear, kLightRampRight, 	purple, 0
					SSetLightColor kModeFear, kLightLoopLeft, 	purple, 0
					SSetLightColor kModeFear, kLightLoopRight, 	purple, 0
					SSetLightColor kModeFear, kLightSpinnerLeft,yellow, 0
					SSetLightColor kModeFear, kLightOrbitLeft, 	yellow, 0
				else 
					SSetLightColor kModeFear, kLightRampLeft, 	purple, 2
					SSetLightColor kModeFear, kLightRampRight, 	purple, 2
					SSetLightColor kModeFear, kLightLoopLeft, 	purple, 2
					SSetLightColor kModeFear, kLightLoopRight, 	purple, 2
					SSetLightColor kModeFear, kLightSpinnerLeft,yellow, 0
					SSetLightColor kModeFear, kLightOrbitLeft, 	yellow, 0
				End if 
			End if 
		elseif lIndex=kLightSpinnerLeft or lIndex=kLightSpinnerRight Then		' Spinner Spinning
			if FearOfTheDarkSpinActive and GetLightState(kModeFear, kLightSpinnerLeft)<>0 then 
				if lIndex=kLightSpinnerLeft then														' Left Spinner is 3x
					FearOfTheDarkSpinTotal=FearOfTheDarkSpinTotal+(FearOfTheDarkSpinnerValue*3)
				else 
					FearOfTheDarkSpinTotal=FearOfTheDarkSpinTotal+FearOfTheDarkSpinnerValue
				End if 
				puPlayer.LabelSet pDMDFull,"Msg5", FormatScore(FearOfTheDarkSpinTotal) & " spun",1,""
			End if 
		else 
			if GetLightState(kModeFear, lIndex)<>0 then
				if lIndex=kLightRampCenter and FearOfTheDarkCount=3 then 
WriteToLog "     ", "Collect Soul Shard"
					' Collect Soul Shard 
					' https://youtu.be/456THtyKy7s?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=7922		- OLD Code ???
					FearOfTheDarkTimer.Enabled = False
					tmrShardHurryup.Enabled=False
					QueueFlushForce 0, True 
					QueueScene2 0, "SceneShard False, kModeFear", 4000, 1, True
					QueueScene2 0, "StopFearOfTheDark", 0, 1, True 

				elseif FearOfTheDarkSpinActive=False then
					AddScoreMode kModeFear, 2000000
					SceneFODWait_Spinners=True 

					QueueFlushForce 0, True 
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""FearOfTheDark"", ""Ramp.mp4"", ""^^^^^^" & FormatScore(2000000) & "^^^"", ""^^^^^^1500:" & pupColorRed & "^^^"" ", 2533, 1
'					QueueScene "SceneClearLabels", 0, 1	

					if lIndex=kLightRampLeft  then FearOfTheDarkSpinnerValue = 250000
					if lIndex=kLightRampRight then FearOfTheDarkSpinnerValue = 250000
					if lIndex=kLightLoopLeft  then FearOfTheDarkSpinnerValue = 350000
					if lIndex=kLightLoopRight then FearOfTheDarkSpinnerValue = 500000
 
					FearOfTheDarkSpinActive=True
					FearOfTheDarkSpinTotal=0
					' Start Spinner 
					SSetLightColor kModeFear, kLightRampLeft, 	purple, 0
					SSetLightColor kModeFear, kLightRampRight, 	purple, 2		' Right Ramp Flashes to double spinner value
					SSetLightColor kModeFear, kLightLoopLeft, 	purple, 0
					SSetLightColor kModeFear, kLightLoopRight, 	purple, 0
					SSetLightColor kModeFear, kLightSpinnerLeft,yellow, 1
					SSetLightColor kModeFear, kLightOrbitLeft, 	yellow, 1
				Elseif FearOfTheDarkSpinActive then								' Double Spinner Value (Right Ramp)
					SSetLightColor kModeFear, kLightRampRight, purple, 0
					FearOfTheDarkSpinnerValue=FearOfTheDarkSpinnerValue*2
					puPlayer.LabelSet pDMDFull,"Msg9", "Spinner Value: " & FormatScore(FearOfTheDarkSpinnerValue) & "/spin",1,""
				End if 

	'			SSetLightColor kModeEddie, lIndex, white, 0
			End If
		End if 
	End if 
End Sub 

'https://youtu.be/zaCEffQkj4o?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=589
'https://youtu.be/p2_jS8GyLSM?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=507
'https://youtu.be/besY8TS0Ges?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1386
dim ProcessAcesNextRamp
Sub ProcessAces(lIndex)
	Dim clipIdx
	dim clipTime
	Dim thisScore
	Dim multStr

	if IsModeActive(kModeAces) then
WriteToLog "     ", "ProcessAces:" & lIndex & " cnt:" & AcesHighCount & " state:" & GetLightState(kModeAces, lIndex)

		if AcesHighCount < 4 then 
			if GetLightState(kModeAces, lIndex)<>0 then
				SSetLightColor kModeAces, lIndex, darkblue, 0
				thisScore = 1250000 + (500000*AcesHighCompletions)

				AcesHighCount = AcesHighCount + 1
				clipIdx=INT(RND*3)+1
				Select case clipIdx
					case 1: clipTime=3134
					case 2: clipTime=2300
					case 3: clipTime=3867
				End select
				PlaySoundVol "vo_fighterJP", VolDef
				QueueFlush 0
				StopSound "sfx_acesLoop"
				StopSound "sfx_acesLock1"
				StopSound "sfx_acesLock2"
				QueueScene "PlaySoundVol ""sfx_aces" & clipIdx & """, VolSfx '" ,0, 1
				if lIndex = kLightRampCenter and BullseyeMultiplier>1 then 
					AddScoreMode kModeAces, thisScore*BullseyeMultiplier
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""Aces" & clipIdx & ".mp4"", ""^^^^^^Fighter Jackpot!^" &  FormatScore(thisScore) & " x " & BullseyeMultiplier & "^" & FormatScore(thisScore*BullseyeMultiplier) & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", clipTime, 1
				Else 
					AddScoreMode kModeAces, thisScore
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""Aces" & clipIdx & ".mp4"", ""^^^^^^Fighter Jackpot!^" &  FormatScore(thisScore) & "^^"", ""^^^^^^^2000:" & pupColorRed & "^^"" ", clipTime, 1
				End if 

				if AcesHighCount=4 then 
					SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 0
					SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightOrbitRight, DarkBlue, 0
				End if 
			End If

		elseif AcesHighCount < 8 then							' DEFEAT THE BOMBERS
			if GetLightState(kModeAces, lIndex)<>0 then
				SSetLightColor kModeAces, lIndex, darkblue, 0
				AcesHighCount = AcesHighCount + 1

				if lIndex=kLightRampLeft or lIndex=kLightRampRight then 
					if lIndex=kLightRampLeft then 
						ProcessAcesNextRamp=kLightRampRight
					else 
						ProcessAcesNextRamp=kLightRampLeft
					End if 
					thisScore=2500000 + (500000*AcesHighCompletions)
					AddScoreMode kModeAces, thisScore

					QueueFlush 0
					StopSound "sfx_acesLoop"
					StopSound "sfx_acesLock1"
					StopSound "sfx_acesLock2"
					QueueScene "PlaySoundVol ""sfx_acesLock" & INT(RND*2)+1 & """, VolSfx '" ,0, 1
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""AcesBomberLockOn.mp4"", ""I:AcesHigh\\mBomberLockedOn.png^^^^^^^^" &  FormatScore(thisScore) & "^"", ""^^^^^^^^1800:" & pupColorRed &"^"" ", 1800, 1

					SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 2
				else 
					SSetLightColor kModeAces, ProcessAcesNextRamp, 	DarkBlue, 2

					Select case AcesHighCount
						case 6: clipIdx=4:clipTime=3867
						case 8: clipIdx=5:clipTime=3034
					End select
					thisScore=2500000 + (500000*AcesHighCompletions)

					PlaySoundVol "vo_bomberJP", VolDef
					QueueFlush 0
					StopSound "sfx_acesLoop"
					StopSound "sfx_acesLock1"
					StopSound "sfx_acesLock2"
					QueueScene "PlaySoundVol ""sfx_aces" & clipIdx & """, VolSfx '" ,0, 1
					if lIndex = kLightRampCenter and BullseyeMultiplier>1 then 
						AddScoreMode kModeAces, thisScore*BullseyeMultiplier
						QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""Aces" & clipIdx & ".mp4"", ""^^^^^^Bomber Jackpot!^" &  FormatScore(thisScore) & " x " & BullseyeMultiplier & "^" & FormatScore(thisScore*BullseyeMultiplier) & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", clipTime, 1
					Else 
						AddScoreMode kModeAces, thisScore
						QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""Aces" & clipIdx & ".mp4"", ""^^^^^^Bomber Jackpot!^" &  FormatScore(thisScore) & "^^"", ""^^^^^^^2000:" & pupColorRed & "^^"" ", clipTime, 1
					End if 

					if AcesHighCount=8 then 	' Start DEFEAT THE ACE 
WriteToLog "     ", "Starting Roving Timer"
						SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 1
						SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 0
						SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 0
						AcesHighTimer.Interval=2000
						AcesHighTimer.UserValue=0
						AcesHighTimer_inc=1
						AcesHighTimer_lIndex=kLightRampLeft
						AcesHighTimer.Enabled=True 
					End if 
					
				End if

			End if
		else 			' DEFEAT THE ACE 
			if GetLightState(kModeAces, lIndex)<>0 then
				SSetLightColor kModeAces, lIndex, darkblue, 0
				AcesHighCount = AcesHighCount + 1

				if AcesHighCount=8 then 
					PlaySoundVol "vo_acesSites", VolDef
					QueueSetDefault 0, "SceneAcesWait", "SceneClearLabels"				' Update the wait clip

				elseif AcesHighCount=9 then 
					QueueSetDefault 0, "SceneAcesWait", "SceneClearLabels"				' Update the wait clip

					SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 2
					SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 0
					SSetLightColor kModeAces, kLightOrbitRight, DarkBlue, 0
					AcesHighTimer.Enabled=False 
					AcesHighTimer.UserValue=0
					AcesHighTimer.Interval=1000
					AcesHighTimer.Enabled=True 
					AddHudInfo kModeAces, "Shoot The", "Ace", "", ":5", False 
				elseif AcesHighCount=10 then
					clipTime=6000
					thisScore=8500000 + (500000*AcesHighCompletions)
					QueueFlush 0
					StopSound "sfx_acesLoop"
					StopSound "sfx_acesLock1"
					StopSound "sfx_acesLock2"
					PlaySoundVol "vo_acesSJP", VolDef
					QueueScene "PlaySoundVol ""sfx_acesSJP"", VolSfx '" ,0, 1
					if lIndex = kLightRampCenter and BullseyeMultiplier>1 then 
						AddScoreMode kModeAces, thisScore*BullseyeMultiplier
						QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""AcesSJP.mp4"", ""^^^^^^Ace Fighter Super Jackpot!^" &  FormatScore(thisScore) & " x " & BullseyeMultiplier & "^" & FormatScore(thisScore*BullseyeMultiplier) & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", clipTime, 1
					Else 
						AddScoreMode kModeAces, thisScore
						QueueScene "SceneGeneralStart pDMDFull, False, False, ""AcesHigh"", ""AcesSJP.mp4"", ""^^^^^^Ace Fighter Super Jackpot!^" &  FormatScore(thisScore) & "^^"", ""^^^^^^^2000:" & pupColorRed & "^^"" ", clipTime, 1
					End if 

					AcesHighCompletions=AcesHighCompletions+1
					' Restart for another Round
					AcesHighTimer.Enabled=False
					RemoveHudInfo kModeAces
					AddHudInfo kModeAces, "Aces", "High", "", "", True
					AcesHighCount=0
					SSetLightColor kModeAces, kLightRampLeft, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightRampRight, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightLoopLeft, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightOrbitLeft, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightRampCenter, DarkBlue, 2
					SSetLightColor kModeAces, kLightLoopRight, 	DarkBlue, 2
					SSetLightColor kModeAces, kLightOrbitRight, DarkBlue, 2

				Elseif tmrShardHurryup.Enabled then 
' Soul Shard collected 
					AcesHighTimer.Enabled = False
					tmrShardHurryup.Enabled=False
					QueueScene2 0, "SceneShard False, kModeAces", 4000, 1, True 
					QueueScene2 0, "StopAcesHigh 0", 0, 1, True
				End if 

			End if 
		End if 
	End if 

End Sub 

'https://youtu.be/zaCEffQkj4o?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1117
Sub ProcessRime(lIndex)
	dim bAddScore:bAddScore=True
	if IsModeActive(kModeRime) then
WriteToLog "     ", "ProcessRime:" & lIndex & " cnt:" & MarinerCount & " state:" & GetLightState(kModeRime, lIndex)

		if lIndex=kLightPharaohTarget and MarinerCount = 0 Then 'Andrew moved this - was Case 8 under Sub pharaoh_target_Hit - the underworld hit is the first challenge in LE version
			PharaohBullseyeFlasherEnabled False
			'stop the counter and lock the value to the Jackpot
			MarinerTimer1.Enabled = 0

			RemoveHudInfo kModeRime
			AddHudInfo kModeRime, "Rime Of The", "Ancient", "Mariner", "", True
			PuPlayer.LabelSet pDMDFull,"Msg9","",1,""

			QueueScene "SceneGeneralStart pDMDFull, False, False, ""RimeOfTheAncientMariner"", ""Albatross Hurryup collected.mp4"", ""^^^^" &  FormatScore(MarinerJPValue) & "^^^^^"", ""^^^^" & pupColorRed & "^^^^^"" ", 4933, 1
			MarinerJPValueStart=MarinerJPValue
			DMD " JACKPOT VALUE IS", CL(1, FormatScore(MarinerJPValue)), "", eNone, eBlink, eNone, 1500, True, "" ' Andrew needs pDMDSplash Value
			lUnderworld.state = 1

			' Just flash bullseye (why??) and not center
			PharaohBullseyeFlash True
			'PharaohBullseyeFlasherEnabled True

			'DMD "_", "  ADDABALL IS LIT", "", eNone, eBlink, eNone, 1500, True, ""

			EnableMummy False			' Cant start mummy
			lOrbArrow.State = 2
			TriggerScript 800, "RampDown : GiEffect(0) : LightEffect(0) : GiOff : DarkLUT"
			Triggerscript 1200, "LightEffect 3"
			TriggerScript 2100, "LightEffect 5"
			TriggerScript 3000, "RampUp : RecallPrevLUT : GIOn: PlaySoundVol ""vo_7waystoloose"", VolDef "

			TriggerScript 3800,"EnableBallSaver DMDFet(kDMDFet_TimeRimeBallSave):RimeStartMB" 						  'shoot blue arrows
			TriggerScript 6000,"AddMultiball 1"	

			MarinerCount = MarinerCount + 1
			CheckMariner
		
		else
			if GetLightState(kModeRime, lIndex)<>0 then
				'SSetLightColor kModeRime, lIndex, blue, 0

				If MarinerCount>=7 and MarinerCount <= 8 then			' Catch the drops 
WriteToLog "     ", "Check Drops"
					if lIndex=kLightLock or lIndex=kLightOrb or lIndex=kLightBonusX then 
WriteToLog "     ", "Got Drops"
						if DTIsDropped(0) and DTIsDropped(1) and DTIsDropped(3) then 
							DTRaise 1
							DTRaise 2
							DTRaise 3
						else 
WriteToLog "     ", "Skipping Drop"
							bAddScore=False 
							MarinerCount=MarinerCount-1
						End if 
					End if 
				End if 

				MarinerCount=MarinerCount+1
WriteToLog "     ", "Check:" & MarinerCount & " " & 6-MarinerDifficulty
				if MarinerCount>=6-MarinerDifficulty and MarinerCount<=6 then 					' See if we hit SJP
					If lIndex=kLightRampCenter then  	' Add progress 
WriteToLog "     ", "Jackpot Phase 1"
						AddScoreMode kModeRime, (MarinerJPValueStart*MarinerJPMult)
						MarinerCount=6
						CheckMariner
						MarinerJPMult=MarinerJPMult+10
						MarinerJPValue=MarinerJPValueStart			' Reset Jackpot 
						bAddScore=False 
						TriggerScript 2000, "RampUp"
						TriggerScript 2800, "Exit_Underworld False "
					End if 
				elseif MarinerCount=10 then 				' See if we hit SJP
					If lIndex=kLightRampCenter then  	' Add progress 
WriteToLog "     ", "Jackpot Phase 2"
						AddScoreMode kModeRime, (MarinerJPValueStart*MarinerJPMult)
						CheckMariner
						MarinerJPMult=MarinerJPMult+10
						MarinerJPValue=MarinerJPValueStart			' Reset Jackpot 
						bAddScore=False 

						TriggerScript 2000, "RampUp"
						TriggerScript 2800, "Exit_Underworld False "

						if MarinerDifficulty=2 then MarinerDifficulty=MarinerDifficulty-1
						MarinerCount=1

						SSetLightColor kModeRime, kLightRampCenter, white, 2
						SSetLightColor kModeRime, kLightRampLeft, 	blue, 0
						SSetLightColor kModeRime, kLightRampRight, 	blue, 0
						SSetLightColor kModeRime, kLightLoopLeft, 	blue, 0
						SSetLightColor kModeRime, kLightLoopRight, 	blue, 0
						SSetLightColor kModeRime, kLightSpinnerLeft,blue, 0
						SSetLightColor kModeRime, kLightOrbitLeft, 	blue, 0
						SSetLightColor kModeRime, kLightLock, 		green, 0
						SSetLightColor kModeRime, kLightOrb, 		purple, 0
						SSetLightColor kModeRime, kLightBonusX, 	yellow, 0
						DTRaise 1
						DTRaise 2
						DTRaise 3
'						Target004.IsDropped = 0 
'						Target005.IsDropped = 0 
'						Target006.IsDropped = 0 
						UpdateDrop
						CheckMariner
					else								' No Progress 
						MarinerCount=9
					End if 
				Elseif tmrShardHurryup.Enabled then	' Soul Shard collected 
					tmrShardHurryup.Enabled=False
					QueueScene2 0, "SceneShard False, kModeRime", 4000, 1, True 
					QueueScene2 0, "StopMariner 0", 0, 1, True 
				End if 

				if bAddScore then 
					AddScoreMode kModeRime, MarinerJPValue
					CheckMariner
					MarinerJPValue=MarinerJPValue+500000
				End if 
			elseif lIndex=kLightPharaohTarget and GetLightState(kModeRime, kLightRampCenter)=0 then 						' Ball must of got knocked back in while ramp was up waiting to eject, kick it out again 
				WriteToLog "     ", "Mariner: Clearing Extra Underworld Lock"
				vpmtimer.addtimer 1000, "Exit_Underworld False '"
			End if 
		End If
	End if 
End Sub 

Sub RimeStartMB
	Exit_Underworld False
	QueueSetDefault 0, "SceneRimeWait", "SceneClearLabels"				' Update the wait clip
'	pupevent 803" 'shoot blue arrows
End Sub 

' FEAR
' https://www.youtube.com/watch?v=p2_jS8GyLSM&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&index=4
' https://www.youtube.com/watch?v=zaCEffQkj4o&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=1452s
' https://www.youtube.com/watch?v=besY8TS0Ges&list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&index=7
Dim ProcessHallowedLastIdx1
Dim ProcessHallowedLastIdx2
Sub ProcessHallowed(lIndex)
	if IsModeActive(kModeHallowed) then
WriteToLog "     ", "ProcessHallowed:" & lIndex & " cnt:" & HallowedCount & " state:" & GetLightState(kModeHallowed, lIndex)

		if lIndex=kLightOrbitLeft2 then lIndex=kLightOrbitLeft ' Partial Orbit counts as the shot

		if GetLightState(kModeHallowed, lIndex)<>0 then
			SSetLightColor kModeHallowed, lIndex, orange, 0
			HallowedCount=HallowedCount+1

			if lIndex=kLightRampLeft then HallowedShots(0)=0
			if lIndex=kLightOrbitLeft then HallowedShots(1)=0
			if lIndex=kLightRampRight then HallowedShots(2)=0
			if lIndex=kLightOrbitRight then HallowedShots(3)=0

			if HallowedCount=1 then
				ProcessHallowedLastIdx1=lIndex
				AddScoreMode kModeHallowed, 3500000
				QueueFlush 0
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP1.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ", 2000, 1
				SSetLightColor kModeHallowed, kLightRampLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightRampRight, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitRight, orange, 0
				SSetLightColor kModeHallowed, kLightCaptiveBall, orange, 2
				SetFastPulse lCaptiveBall
			elseif HallowedCount=2 then
				AddScoreMode kModeHallowed, 5000000
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP2.mp4"", ""^^^^^^Escape Awarded!^^" & FormatScore(5000000) & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", 2000, 1

				SSetLightColor kModeHallowed, kLightRampLeft, 	orange, HallowedShots(0)
				SSetLightColor kModeHallowed, kLightRampRight, 	orange, HallowedShots(2)
				SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, HallowedShots(1)
				SSetLightColor kModeHallowed, kLightOrbitRight, orange, HallowedShots(3)
				SSetLightColor kModeHallowed, kLightCaptiveBall, orange, 0
				SetDefPulse lCaptiveBall
				UpdateMummy2
				SSetLightColor kModeHallowed, ProcessHallowedLastIdx1, orange, 0		' Turn off the first one they hit 

			elseif HallowedCount=3 then
				ProcessHallowedLastIdx2=lIndex
				AddScoreMode kModeHallowed, 5000000
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP3.mp4"", ""^^^^Escape Shot Lit!^^^^^"", ""^^^^^^^^^"" ", 2000, 1
				SSetLightColor kModeHallowed, kLightRampLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightRampRight, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitRight, orange, 0
				SSetLightColor kModeHallowed, kLightLock, green, 2
				SSetLightColor kModeHallowed, kLightOrb, purple, 2
				SSetLightColor kModeHallowed, kLightBonusX, yellow, 2
				DTRaise 1
				DTRaise 2
				DTRaise 3
'				Target004.IsDropped = 0 
'				Target005.IsDropped = 0 
'				Target006.IsDropped = 0 
			elseif HallowedCount=4 then 
WriteToLog "     ", "Hallowed 4:" & DTIsDropped(1) & " " & DTIsDropped(2) & " " & DTIsDropped(3)
				if DTIsDropped(1) and DTIsDropped(2) and DTIsDropped(3) then 
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP4.mp4"", ""^^^^^^Escape Awarded!^^" & FormatScore(10000000) & "^"", ""^^^^^^^^2000:" & pupColorRed & "^"" ", 2000, 1
					AddScoreMode kModeHallowed, 10000000
					SSetLightColor kModeHallowed, kLightRampLeft, 	orange, HallowedShots(0)
					SSetLightColor kModeHallowed, kLightRampRight, 	orange, HallowedShots(2)
					SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, HallowedShots(1)
					SSetLightColor kModeHallowed, kLightOrbitRight, orange, HallowedShots(3)

					DTRaise 1
					DTRaise 2
					DTRaise 3
'					Target004.IsDropped = 0 
'					Target005.IsDropped = 0 
'					Target006.IsDropped = 0 
					UpdateDrop
				else 
					AddScoreMode kModeHallowed, 60000
					HallowedCount=3
				End if 

			elseif HallowedCount=5 then 	' Escape Shot Lit
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP5.mp4"", ""^^^^^^^^^"", ""^^^^^^^^^"" ", 2000, 1
				SSetLightColor kModeHallowed, kLightRampLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightRampRight, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitLeft, 	orange, 0
				SSetLightColor kModeHallowed, kLightOrbitRight, orange, 0
				SSetLightColor kModeHallowed, kLightRampCenter, orange, 2

			elseif HallowedCount=6 then 	' Escape Shot Lit
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""HallowedBeThyName"", ""HallowedP6.mp4"", ""^^Escape Awarded!^^" & FormatScore(20000000) & "^^Escape Bonus!^^" & FormatScore(16000000) & "^"", ""^^^^^^" & pupColorRed & "^^" & pupColorRed & "^"" ", 3000, 1

				HallowedTimer.Enabled=False		' Stop Mode and start hurry up
				AddScoreMode kModeHallowed, 20000000
				StartShardHurryup(kModeHallowed)
				SSetLightColor kModeHallowed, kLightRampCenter, white, 2

			elseif HallowedCount=7 then 	' Shard Collect 
				HallowedTimer.Enabled=False
				tmrShardHurryup.Enabled=False
				QueueScene2 0, "SceneShard False, kModeHallowed", 4000, 1, True
				QueueScene2 0, "StopHallowed", 0, 1, True 
			end if 

		End if 
	End if 

End Sub 


Sub ProcessIcarus(lIndex)
	Dim BonusStr:BonusStr=""
	dim videoStr
	Dim thisScore
	if IsModeActive(kModeIcarus) then
WriteToLog "     ", "ProcessIcarus:" & lIndex & " cnt:" & IcarusMultiplier & " state:" & GetLightState(kModeIcarus, lIndex)
		if GetLightState(kModeIcarus, lIndex)<>0 then
			SSetLightColor kModeIcarus, lIndex, orange, 0

			if lIndex = kLightRampCenter then 		' Center is only Lit for Soul Shard 
				if IcarusTimer.Enabled then 		' Start Soul Shard 
					IcarusTimer.Enabled=False
					IcarusCombo.Enabled=False
					thisScore=1000000
					AddScore thisScore
					QueueScene "SceneGeneralStart pDMDFull, False, False, ""FlightOfIcarus"", ""FlightEarly.mp4"", ""I:FlightOfIcarus\\txt4.png^^^^^" & BonusStr & "^^" & FormatScore(thisScore) & "^^"", ""^^^^^^^2000:" & pupColorRed & "^^"" ", 2000, 1
					StartShardHurryup(kModeIcarus)
					SSetLightColor kModeIcarus, kLightRampCenter, white, 2	
				else 								' Collect Soul Shard 
					IcarusTimer.Enabled=False
					tmrShardHurryup.Enabled=False
					QueueScene2 0, "SceneShard False, kModeIcarus", 4000, 1, True 
					QueueScene2 0, "StopFlightOfIcarus", 0, 1, True
				End if 
			else 
				if IcarusMultiplier<>1 then 
					BonusStr="8:" & IcarusMultiplier & "X COMBO"
				End if 
				if lIndex=kLightRampLeft then 
					videoStr="Left Ramp.mp4"
					SSetLightColor kModeIcarus, kLightRampRight, yellow, 2
				else 
					videoStr="Right Ramp.mp4"
					SSetLightColor kModeIcarus, kLightRampLeft, yellow, 2
				End if 

				QueueFlush 0
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""FlightOfIcarus"", """&videoStr&""", ""^^^^^" & BonusStr & "^^" & FormatScore(IcarusValue*IcarusMultiplier) & "^^"", ""^^^^^" & pupColorBlue & "^^" & pupColorRed & "^^"" ", 2000, 1
				DMD "ICARUS COMBO", CL(1, FormatScore(IcarusValue) & " X " & IcarusMultiplier), "", eNone, eBlink, eNone, 1500, True, "" 
				AddScoreMode kModeIcarus, IcarusValue*IcarusMultiplier
				IcarusValue=IcarusValue+250000
				IcarusMultiplier=IcarusMultiplier+1

				if ModePoints(CurrentPlayer, kModeIcarus) > 20000000 then 		' Light center shot to end 
					SSetLightColor kModeIcarus, kLightRampCenter, yellow, 2
					puPlayer.LabelSet pDMDFull,"Msg9", "Soul Shard Qualified!",1,""
				End if 

				IcarusCombo.Enabled=False 		' Reset 
				IcarusCombo.Enabled=True
			End if 
		else
			if lIndex=kLightRampLeft or lIndex=kLightRampRight then 
				IcarusCombo.Enabled=False 		' Reset timer  
				IcarusCombo.Enabled=True
			End if 
		End if 
	End if 
End Sub 

'https://youtu.be/besY8TS0Ges?list=PLjDd1PAVOmdBhW113WXCHKfb-TqbOvCb5&t=6893
Sub Process2M2M(lIndex)
	dim rndIdx
	Dim SJPBonus
	Dim videoStr
	Dim audioStr
	Dim Score
	Dim bValidHit:bValidHit=False
	Dim SaveIdx:SaveIdx=-1

	if IsModeActive(kMode2M2M) then

		if TwoMinToMidnightTimer.Enabled = False then 		' 1st switchhit starts the timer 
			TwoMinToMidnightTimer.Interval = 1
			TwoMinToMidnightTimer.Enabled = True
		End if 

WriteToLog "     ", "Process2M2M:" & lIndex & " cnt:" & TwoMinutesToMidnightHitCount & " " & GetLightState(kMode2M2M, lIndex)

		if lIndex = kLightOrbitLeft2 then 
			SaveIdx=kLightOrbitLeft2
			lIndex=kLightOrbitLeft

			if GetLightState(kMode2M2M, kLightSJP)<>0 then	' SJP is lit 
				post001_IsDropped(False)
				VPMTimer.AddTimer 1000, "post001_IsDropped(True) '"
			End if 
		End if 

		if GetLightState(kMode2M2M, lIndex)<>0 then
			SSetLightColor kMode2M2M, lIndex, white, 0

			if lIndex>=kLightTargetX1 and lindex<=kLightTargetX4 then 	' Handle X lights add time
				QueueScene "PlaySoundVol ""sfx_2m2m_clock" & INT(RND*2)+1& """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", ""2m2m_add"&INT(RND*4)+1&".mp4"", ""^^8:5 SECONDS ADDED^^^^^^^"", ""^^" & pupColorRed & "^^^^^^^"" ", 1367, 1
				TwoMinToMidnightTimerCount = TwoMinToMidnightTimerCount - 10
				PauseTimers 2000

				' If they are all out we just light 1
				if (GetLightState(kMode2M2M, kLightTargetX1)=0 and GetLightState(kMode2M2M, kLightTargetX2)=0 and _
				    GetLightState(kMode2M2M, kLightTargetX3)=0 and GetLightState(kMode2M2M, kLightTargetX4)=0) then 
					'Relight 1 X Shot 
					SSetLightColor kMode2M2M, kLightTargetX1, red, 0
					SSetLightColor kMode2M2M, kLightTargetX2, red, 0
					SSetLightColor kMode2M2M, kLightTargetX3, red, 0
'					SSetLightColor kMode2M2M, kLightTargetX4, red, 0			' I dont thin kwe light the one on the SJP lane so they can keep increasing
					Select case INT(RND*3)
						case 0: SSetLightColor kMode2M2M, kLightTargetX1, red, 2
						case 1: SSetLightColor kMode2M2M, kLightTargetX2, red, 2
						case 2: SSetLightColor kMode2M2M, kLightTargetX3, red, 2
'						case 3: SSetLightColor kMode2M2M, kLightTargetX4, red, 2
					End Select 
				End if 

			End if 
			rndIdx=INT(RND*6)+1
			videoStr="2m2m_p" & rndIdx 
			audioStr="sfx_2m2m_rocket" & rndIdx 

			if lIndex=kLightSpinnerLeft then ' Fear Of the Dark
				bValidHit=True 
				Score=1000000+INT(ModePoints(CurrentPlayer, kModeFear)/100)*15						' 15% of score
'				Score=1000000+0.15*ModePoints(CurrentPlayer, kModeFear)
				AddScoreMode kMode2M2M, Score
				QueueFlush 0
				QueueScene "PlaySoundVol """ & audioStr & """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", """&videoStr&".mp4"", ""^^^FEAR OF THE DARK^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 2000, 1

			elseif lIndex=kLightRampLeft then ' Flight Of Icarus 
				bValidHit=True 
				Score=1000000+INT(ModePoints(CurrentPlayer, kModeIcarus)/100)*15					' 15% of score
'				Score=1000000+0.15*ModePoints(CurrentPlayer, kModeIcarus)
				AddScoreMode kMode2M2M, Score
				QueueFlush 0
				QueueScene "PlaySoundVol """ & audioStr & """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", """&videoStr&".mp4"", ""^^^FLIGHT OF ICARUS^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 2000, 1

			elseif lIndex=kLightRampRight then ' ACES High 
				bValidHit=True 
				Score=1000000+INT(ModePoints(CurrentPlayer, kModeAces)/100)*15						' 15% of score
'				Score=1000000+0.15*ModePoints(CurrentPlayer, kModeAces)
				AddScoreMode kMode2M2M, Score
				QueueFlush 0
				QueueScene "PlaySoundVol """ & audioStr & """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", """&videoStr&".mp4"", ""^^^ACES HIGH^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 2000, 1

			elseif SaveIdx=kLightOrbitLeft2 then ' Hallowed and ball was going up
				bValidHit=True 
				Score=1000000+INT(ModePoints(CurrentPlayer, kModeHallowed)/100)*15					' 15% of score
'				Score=1000000+0.15*ModePoints(CurrentPlayer, kModeHallowed)
				AddScoreMode kMode2M2M, Score
				QueueFlush 0
				QueueScene "PlaySoundVol """ & audioStr & """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", """&videoStr&".mp4"", ""^^^HALLOWED BE THY NAME^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 2000, 1

			elseif lIndex=kLightOrbitRight then ' Rime
				bValidHit=True 
				Score=1000000+INT(ModePoints(CurrentPlayer, kModeRime)/100)*15						' 15% of score
'				Score=1000000+0.15*ModePoints(CurrentPlayer, kModeRime)
				AddScoreMode kMode2M2M, Score
				QueueFlush 0
				QueueScene "PlaySoundVol """ & audioStr & """,VolSfx", 1, 1 
				QueueScene "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", """&videoStr&".mp4"", ""^^^5:RIME OF THE ANCIENT MARINER^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 2000, 1

			elseif lIndex=kLightSJP then ' Super Jackpot
				if TwoMinutesToMidnightBonusMult>=2 then 
					Score = TwoMinutesToMidnightBonusMult*ModePoints(CurrentPlayer, kMode2M2M)
				else 
					Score = ModePoints(CurrentPlayer, kMode2M2M)
				End if 
				AddScore Score		' DOES SJP GET ADDED TO MODE TOTAL - IF SO I NEED A DIFFERENT VARIABLE to TRACK SJP
				QueueFlush 0
				QueueScene "PlaySoundVol ""sfx_2m2m_sjp"",VolSfx", 1, 1 
				QueueScene "PlaySoundVol ""vo_superjackpot"",VolSfx", 1, 1 

				if TwoMinutesToMidnightBonusMult>=2 then 
					QueueScene2 0, "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", ""2m2m_sjp.mp4"", ""^^^5:SUPER JACKPOT^^"& FormatScore(ModePoints(CurrentPlayer, kMode2M2M)) &  " X " & TwoMinutesToMidnightBonusMult & "^^"& FormatScore(Score) & "^^"", ""^^^^^^^"&pupColorRed&"^^"" ", 4000, 1, True
					AddEddieCard kMode2M2M, True 	' Start card animation
				Else 
					QueueScene2 0, "SceneGeneralStart pDMDFull, False, False, ""Two Minutes to Midnight"", ""2m2m_sjp.mp4"", ""^^^5:SUPER JACKPOT^^"& FormatScore(Score) & "^^^^"", ""^^^^^"&pupColorRed&"^^^^"" ", 4000, 1, True 
				End if 

				TwoMinutesToMidnightBonusMult=0
			End if

			' Track hits 
			if bValidHit then
				TwoMinutesToMidnightHitCount = TwoMinutesToMidnightHitCount + 1
				if TwoMinutesToMidnightHitCount=5 then 
					TwoMinutesToMidnightBonusMult=TwoMinutesToMidnightBonusMult+1
					SSetLightColor kMode2M2M, kLightSJP, red, 2

					' Relight shots to go for double JP
					TwoMinutesToMidnightHitCount=0
					SSetLightColor kMode2M2M, kLightRampLeft, 	yellow, 2
					SSetLightColor kMode2M2M, kLightRampRight, 	blue, 2
					SSetLightColor kMode2M2M, kLightOrbitLeft, 	orange, 2
					SSetLightColor kMode2M2M, kLightSpinnerLeft,purple, 2
					SSetLightColor kMode2M2M, kLightOrbitRight,  teal, 2

				End if
			End if

		End if 
	End If 
End Sub

Sub StartShardHurryUp(Mode)
	dim SaveTxt1:SaveTxt1=tmrHUDAnimate_InitialValue1
	Dim SaveTxt2:SaveTxt2=tmrHUDAnimate_InitialValue2
WriteToLog "     ", "StartShardHurryUp:" & ModePoints(CurrentPlayer, Mode)*.2

	QueueSetDefault 0, "SceneShard True, " & Mode, "SceneClearLabels"

'	RemoveHudInfo Mode
	AddHudInfo Mode, "SOUL SHARD", "HURRY UP", "", ":10", False 				' Replace the current HUD with  sould shard countdown
	If IsModeQual(kModeTrooper)=False and IsModeQual(kModeMummy)=False then		' If we are stacked then dont overwrite the Upper HUD
		AddHudInfo Mode, SaveTxt1, SaveTxt2, "", "", True
	End if

	SetLightColor lUnderworld, red, 2
	' TBD Set Wall flasher RED 
	QueueScene2 0, "StartShardHurryUp2 " & mode, 100, 1, True		' delay the timer until the scene starts 
End Sub

Sub StartShardHurryUp2(Mode)
	dim holdModeScore
	tmrShardMode=Mode
	tmrShardHurryupCnt=0

	' Soul shard is 20% of score - 500 so it doesnt end on 0
	holdModeScore = (INT(ModePoints(CurrentPlayer, Mode)/100)*20) - 500
	tmrShardHurryupDecrement=holdModeScore\50
'	PuPlayer.LabelSet pDMDFull,"ModeTxtL1", "SOUL SHARD", 1, " "
'	PuPlayer.LabelSet pDMDFull,"ModeTxtL2", "HURRY UP", 1, " "
'	PuPlayer.LabelSet pDMDFull,"ModeTimer", "", 1,""
	tmrShardHurryup.Interval=2000										' Initial Delay 
	tmrShardHurryup.UserValue=INT(ModePoints(CurrentPlayer, Mode)/100)*20		' Hurryup starts at 20% of mode points 
	tmrShardHurryup.Enabled=True
End Sub

Dim tmrShardMode
Dim tmrShardHurryupDecrement
Dim tmrShardHurryupCnt
Sub tmrShardHurryup_Timer
	Dim TimerVal
WriteToLog "     ", "tmrShardHurryup_Timer:"& tmrShardHurryup.UserValue

	tmrShardHurryup.Interval=200
	tmrShardHurryupCnt=tmrShardHurryupCnt+1
	TimerVal=10-(tmrShardHurryupCnt\5)
	tmrShardHurryup.UserValue=tmrShardHurryup.UserValue-tmrShardHurryupDecrement
	puPlayer.LabelSet pDMDFull,"Msg5", FormatScore(tmrShardHurryup.UserValue), 1, ""
	if tmrMummyMBHurryup.Enabled=False then			' Mummy Hurryup/countdown isnt running
		PuPlayer.LabelSet pDMDFull,"ModeTimer", ":" & TimerVal, 1,""       ' Start FOTD timer count - Chris
		UpdateClock TimerVal
	End if 

	if tmrShardHurryupCnt=50 then 	' 10 seconds 
		tmrShardHurryup.Enabled=False 
		Select case tmrShardMode
			Case kModeFear:		QueueScene "StopFearOfTheDark", 0, 1
			Case kModeAces:		QueueScene "StopAcesHigh 0", 0, 1
			Case kModeIcarus:	QueueScene "StopFlightOfIcarus", 0, 1
			Case kModeHallowed:	QueueScene "StopHallowed", 0, 1
			Case kModeRime:		QueueScene "StopMariner 0", 0, 1
		End Select
	End if 
End Sub 


Sub pClearScore
	puPlayer.LabelSet pDMDFull,"Credits"," " ,1,""
	puPlayer.LabelSet pDMDFull,"Ball"," ",1,""
'	puPlayer.LabelSet pDMDFull,"CurrentPlayer"," ",1,""
	puPlayer.LabelSet pDMDFull,"CurrentPlayerScore"," ",1,""
End Sub

Sub pClearPowerFeatures()
WriteToLog "     ", "pClearPowerFeatures"
'	PuPlayer.LabelSet pDMDFull,"BonusSpinners","",1,"" 
'	PuPlayer.LabelSet pDMDFull,"BonusRamps","",1,"" 
'	PuPlayer.LabelSet pDMDFull,"BonusPops","",1,"" '
'	PuPlayer.LabelSet pDMDFull,"BonusTargets","",1,"" 

	PuPlayer.LabelSet pDMDFull,"RemainingSpinners"," ",1,""
	PuPlayer.LabelSet pDMDFull,"RemainingRamps"," ",1,""
	PuPlayer.LabelSet pDMDFull,"RemainingPops"," ",1,""
	PuPlayer.LabelSet pDMDFull,"RemainingTargets"," ",1,""
	PuPlayer.LabelSet pDMDFull,"RemainingOrbits"," ",1,""
	puPlayer.LabelSet pDMDFull,"pSpinnersChk","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pPopsChk","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pOrbitsChk","PuPOverlays\\clear .png",1,""
	puPlayer.LabelSet pDMDFull,"pRampsChk","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pTargetsChk","PuPOverlays\\clear.png",1,""

	puPlayer.LabelSet pDMDFull,"pSpinners","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pRamps","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pOrbits","PuPOverlays\\clear .png",1,""
	puPlayer.LabelSet pDMDFull,"pTargets","PuPOverlays\\clear.png",1,""
	puPlayer.LabelSet pDMDFull,"pPops","PuPOverlays\\clear.png",1,""
	
	puPlayer.LabelSet pDMDFull,"pOverlay","PuPOverlays\\clear.png",1,""
end Sub

	
Sub pCountdownTimer
	
	PuPlayer.LabelSet pDMDFull,"TwoMinutesToMidnightCountdown",TwoMinToMidnightTimerCount,1,""
	'PuPlayer.LabelSet pDMDFull,"OrbitsCountdown",THERE ISNT ONE YET,1""
	'PuPlayer.LabelSet pDMDFull,"OrbitsJackpotCountdown",THERE ISN"T ONE YET,1""
	'PuPlayer.LabelSet pDMDFull,"OrbitsJackpotCountdown",THERE ISN"T ONE YET,1""
	'PuPlayer.LabelSet pDMDFull,"MadnessCountdown",THERE ISN"T ONE YET,1""
end Sub



PUPInit  'this should be called in table1_init at bottom after all else b2s/controller running.


'********************  pretty much only use pupDMDDisplay all over ************************   
' Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
'Samples
'pupDMDDisplay "shoot", "SHOOT AGAIN!", ", 3, 1, 10 
'pupDMDDisplay "default", "DATA GADGET LIT", "@DataGadgetLit.mp4", 3, 1, 10
'pupDMDDisplay "shoot", "SHOOT AGAIN!", "@shootagain.mp4", 3, 1, 10   
'pupDMDDisplay "balllock", "Ball^Locked|16744448", "", 5, 1, 10             '  5 seconds,  1=flash, 10=priority, ball is first line, locked on second and locked has custom color |
'pupDMDDisplay "balllock","Ball 2^is^Locked", "balllocked2.mp4",3, 1,10     '  3 seconds,  1=flash, play balllocked2.mp4 from dmdsplash folder, 
'pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,10       '  3 seconds,  1=flash, play @balllocked.mp4 from dmdsplash folder, because @ text by default is hidden unless useDmDvideos is disabled.


'pupDMDDisplay "shownum", "3^More To|616744448^GOOOO", "", 5, 1, 10         ' "shownum" is special.  layout is line1=BIG NUMBER and line2,line3 are side two lines.  "4^Ramps^Left"

'pupDMDDisplay "target", "POTTER^110120", "blank.mp4", 10, 0, 10            ' 'target'...  first string is line,  second is 0=off,1=already on, 2=flash on for each character in line (count must match)



'NailBusters TriggerScript based on time

Const TriggerScriptSize=20
Dim pReset(20) 				' TriggerScriptSize
Dim pStatement(20)          ' TriggerScriptSize - holds future scripts
Dim FX

for fx=0 to TriggerScriptSize
    pReset(FX)=0
    pStatement(FX)=""
next

DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval

Sub pTriggerScript_Timer()
	dim bMoreToRun:bMoreToRun=False
	for fx=0 to TriggerScriptSize  
		if pReset(fx)>0 Then	
			pReset(fx)=pReset(fx)-pTriggerCounter 
			if pReset(fx)<=0 Then
WriteToLog "     ", "Running :" & fx & " " & pStatement(fx)
				pReset(fx)=0
				execute(pStatement(fx))
			else 
				bMoreToRun=True
			end if
		End if
	next

	if bMoreToRun = False then pTriggerScript.Enabled=False	' Disable when we dont need it 

End Sub


Sub TriggerScript(pTimeMS, pScript) ' This is used to Trigger script after the pTriggerScript Timer on the playfield expires
'WriteToLog "     ", "TriggerScript: " & pScript
	for fx=0 to TriggerScriptSize  
		if pReset(fx)=0 Then
			pReset(fx)=pTimeMS
			pStatement(fx)=pScript
			pTriggerScript.Enabled=True
			Exit Sub
		End If 
	next
	WriteToLog "     ", "ERROR: No TriggerScripts Left!!!!!!!!!!!!!!!!!!!!"
end Sub


'************************************************************************
' ************  DAPHISHBOWL SCENE ROUTINE
'************************************************************************

Sub SceneClearLabels()
WriteToLog "     ", "SceneClearLabels"
	' Clear Labels and flash (Just in case it is flashing)
	PuPlayer.LabelSet pDMDFull,"MsgFull","PupOverlays\\clear.png",1,""
' PUP BUG? - Clearing the flashing text with mt:1 this sometimes causes the next video to not play
'    Looks like this was <3 second video lengths, not sure why this changed anything
'	PuPlayer.LabelSet pDMDFull,"Msg1"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg2"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg3"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg4"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg5"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg6"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg7"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg8"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg9"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"
'	PuPlayer.LabelSet pDMDFull,"Msg10"," ",1,"{'mt':1,'at':1,'fq':150,'len':0}"

	PuPlayer.LabelSet pDMDFull,"Msg1"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg2"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg3"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg4"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg5"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg6"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg7"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg8"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg9"," ",1,""
	PuPlayer.LabelSet pDMDFull,"Msg10"," ",1,""

	' clear 2m2m
	if IsModeActive(kMode2M2M) then 
		PuPlayer.LabelSet pDMDFull,"M2M_CNTBG","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT0","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT1","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT2","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT3","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT4","PupOverlays\\clear.png",1,""
		PuPlayer.LabelSet pDMDFull,"M2M_CNT5","PupOverlays\\clear.png",1,""
	End if 

End Sub 

Sub SceneImage(ImagePath)
	PuPlayer.LabelSet pDMDFull,"MsgFull" , ImagePath, 1,"{'mt':2,'height':100,'width':100}"
End Sub 


Sub SceneGeneralStartDef(bLoop, bMute, VideoPath, VideoName, Messages)	'Size:Msg1^Msg2^Msg3^Msg4^Msg5^Msg6^Msg7^Msg8^Msg9^Msg10
	SceneGeneralStart pDMDFull, bLoop, bMute, VideoPath, VideoName, Messages, ""
End Sub 

Sub SceneGeneralStart(Screen, bLoop, bMute, VideoPath, VideoName, Messages, MsgColors)	'Size:Msg1^Msg2^Msg3^Msg4^Msg5^Msg6^Msg7^Msg8^Msg9^Msg10
	Dim vol
	dim msgs, bSkipMessages
	dim colors
	dim fontFlash(10)
	dim fontSize(10)
	Dim i
	Dim tmp
	Dim tmpEsc
	Dim ImageText:ImageText=""

WriteToLog "     ", "SceneGeneralStart: VideoPath:" & VideoPath & " MSG:" & Messages
'WriteToLog "     ", "Colors:" & MsgColors

	if MsgColors<> "" then 
		colors = Split(MsgColors, "^")
	else 
		colors = Split("^^^^^^^^^", "^")
	End if 
	for i = 0 to 9
		if colors(i)="" then 
			colors(i)=RGB(200,197,200) ' RGB(160, 194, 235)
		else 
			tmp = Split(colors(i),":")			' Adding 1000:RGB(x,y,z)  with flash the text for 1000msec
			if ubound(tmp)=1 then
				colors(i)=tmp(1)
				fontFlash(i)=tmp(0)
			else 
				fontFlash(i)=""
			End if 
		End if 

	Next 

	if Messages <> "" then 									' Split messages and sizes
		msgs = Split(Messages, "^")
		bSkipMessages=False
		for i = 0 to 9
			tmpEsc=Replace(msgs(i), "\:", ";;")				' Handle Escaping
			tmp = Split(tmpEsc,":")
			if ubound(tmp)=1 then 
				if IsNumeric(tmp(0)) then 					' Make sure it is a number for font size
					fontSize(i)=tmp(0)
					tmpEsc=Replace(tmp(1), ";;", ":")
					msgs(i)=tmpEsc
				Else
					fontSize(i)=6

					if tmp(0)="I" then 						 ' Use an image for the message
						ImageText=tmp(1)
						msgs(i)=""
					End if 

				End if 
			else 
				if len(msgs(i))>=25 then fontSize(i)=3.5		' Shrink fonts for Shame an others if they dont specify a size 
				fontSize(i)=6
			End if 
			if msgs(i)<>"" then msgs(i)=" "&msgs(i)&" "		' Add space around it so shadows are not cut off
		Next 
	Else
		bSkipMessages=True
	End if 

	if bMute then 	' These videos are muted
		vol=0
	Else
		vol=1
	End If

	if bLoop=False then 			' If a Persistant mode is active the hide it until I go back to the default loop
		HideActiveModes		
	else 
		ShowActiveModes
	End if

	if VideoName <> "" then 		' Allows us to just set text without changing video 
		playclear Screen
		playmedia VideoName, VideoPath, Screen, "", -1, "", vol, 1
		if bLoop then 
			'PuPlayer.SetBackGround screen, 1
			PuPlayer.SetLoop Screen, 1
		End If 
	End if 

	if bSkipMessages = False then 
		if ImageText<>"" then 
			PuPlayer.LabelSet pDMDFull,"MsgFull" ,ImageText, 1,"{'mt':2,'height':100,'width':100}"
		else 
			WriteToLog "     ", "SKIP Message"
			PuPlayer.LabelSet pDMDFull,"MsgFull","PupOverlays\\clear.png",1,""
		End if 

		for i = 0 to 9 
			PuPlayer.LabelSet Screen,"Msg"&i+1,msgs(i), 1,"{'mt':2,'color':"&colors(i)&",'size':"&fontSize(i)&",'shadowcolor':0, 'shadowstate': 1, 'xoffset': 3, 'yoffset': 3,'outline':1}"
'			if fontFlash(i)<>"" then 
'				PuPlayer.LabelSet pDMDFull, "Msg" & i+1, msgs(i),1,"{'mt':1,'at':1,'fq':150,'len':" & fontFlash(i) & "}"
'			End if 
		Next 

	End if 


End Sub


'************************************************************************
' ************  DAPHISHBOWL QUEUE ROUTINES
'************************************************************************
' Queue - This could be used for anything but I use it to queue  priority=1 items up with the option to have 1 Priority=2 item queued or running 
'				Thought here is Pri 1 items need to be shown, Pri 2 items can be shown if an item is running 
'
' 	NOTE - Since VPMtimer is limited to 20 concurrent timers you need a timer called tmrQueue to ensure items dont get dropped 
'	QueueScene
'		Command=vbscript command    ex:   "RunFunction ""Test"", 123  '"
'		Length=milliseconds before running the next item in the queue
'		Priority=Number, 0 being highest
'
'  NOTE: Declared 2 queues but probably only need 1
'
'*********
dim PupQueue(4, 20, 4)		' QueueNum,   Size=20,  Fields are 0=Command, 1=Priority, 2=time, 3=MustRun
dim PupQueueEndPos(4)		' Size of each queue (-1 = Empty)
dim QueueActive(4)			' We are actively running something 
Dim QueueCurrentTime(4)		' How much time is this one going to run (Just used for getting the queue time)
Dim PupQueueDefault(4)		' Default function to run when the queue is empty
Dim PupQueueDefaultClear(4)		' Default function to run when a new item it added to the queue
QueueActive(0)=False
QueueActive(1)=False
QueueActive(2)=False
QueueActive(3)=False
PupQueueEndPos(0)=-1
PupQueueEndPos(1)=-1
PupQueueEndPos(2)=-1
PupQueueEndPos(3)=-1
PupQueueDefault(0)=""
PupQueueDefault(1)=""
PupQueueDefault(2)=""
PupQueueDefault(3)=""


Sub QueueSetDefault(queueIdx, Command, CommandClear)
WriteToLog "     ", "QueueSetDefault:" & Command

	PupQueueDefault(queueIdx)=""
	PupQueueDefaultClear(queueIdx)=""
	if Command<>"" then PupQueueDefault(queueIdx)=Command & " '"
	if CommandClear<>"" then PupQueueDefaultClear(queueIdx)=CommandClear & " '"
	if QueueActive(queueIdx) = False then 
		Execute Command
	End if 
End Sub 

Sub QueueStartDefault()
	Dim queueIdx:queueIdx=0

	if PupQueueDefault(queueIdx)<>"" and PupQueueDefaultClear(queueIdx)	<>"" then	' clear the default if there is one 
WriteToLog "     ", "Execute Clear:" & PupQueueDefaultClear(queueIdx)
		Execute PupQueueDefaultClear(queueIdx)
	End if 

WriteToLog "     ", "Queue Empty Deactivated: " & PupQueueDefault(queueIdx)
	if PupQueueDefault(queueIdx)<>"" then 		' Run the default item
		Execute PupQueueDefault(queueIdx)
	End if 

End Sub 

Sub QueueFlush(queueIdx)
	QueueFlushForce queueIdx, False 
End Sub 
Sub QueueFlushForce(queueIdx, bForce)
	Dim time
	dim xx
	dim nextFree
WriteToLog "     ", "QueueFlush: " & queueIdx & " " & bForce

	nextFree=-1
	for xx = 0 to PupQueueEndPos(queueIdx)
		if PupQueue(queueIdx, xx, 3)=True then		' MustRun=True,  keep it 
			if nextFree=-1 then 
				nextFree=0
			else 
				nextFree=nextFree+1
			End if 
			PupQueue(queueIdx, nextFree, 0 )=PupQueue(queueIdx, xx,0 )
			PupQueue(queueIdx, nextFree, 1 )=PupQueue(queueIdx, xx,1 )
			PupQueue(queueIdx, nextFree, 2 )=PupQueue(queueIdx, xx,2 )
			PupQueue(queueIdx, nextFree, 3 )=PupQueue(queueIdx, xx,3 )
		End if 
	Next 
	PupQueueEndPos(queueIdx)=nextFree

WriteToLog "     ", "--Q-Dump Flush---"
	for xx = 0 to PupQueueEndPos(queueIdx)
		WriteToLog "     ", xx & " " & PupQueue(queueIdx, xx, 0) & " " & PupQueue(queueIdx, xx, 1) & " " & PupQueue(queueIdx, xx, 2) & " " & PupQueue(queueIdx, xx, 3) 
	Next 
WriteToLog "     ", "--Q-Dump Flush---"

	' See if one is actively running
	time=0
	if QueueActive(queueIdx) and QueueCurrentTime(queueIdx) <> 0 then time = (DateDiff("s", now, QueueCurrentTime(queueIdx)) * 1000)

	if (nextFree= -1 and time=0) or bForce then
WriteToLog "     ", "QueueFlush Empty Deacivated"
		QueueActive(queueIdx)=False
		tmrQueues(queueIdx).Enabled = False
	End if 
End Sub

Function getQueueTime(queueIdx)		' Returns how much time left on queue
	Dim time,i 
	time = 0
WriteToLog "     ", "GetQueueTime: (" & queueIdx &") " & now 
WriteToLog "     ", "GetQueueTime:" & QueueCurrentTime(queueIdx) & " " & QueueActive(queueIdx)
	if QueueActive(queueIdx) and QueueCurrentTime(queueIdx) <> 0 then time = (DateDiff("s", now, QueueCurrentTime(queueIdx)) * 1000)
WriteToLog "     ", "GetQueueTime Active:" & time

	for i = 0 to PupQueueEndPos(queueIdx)
WriteToLog "     ", i & "    " & PupQueue(queueIdx, i, 0) & " " & PupQueue(queueIdx, i, 1) & " " & PupQueue(queueIdx, i, 2) & " " & PupQueue(queueIdx, i, 3) 
		time = time + PupQueue(queueIdx, i, 2) 
	Next
	if time <> 0 then  time=time+1000 ' Add a second because fidelity is 1 second but queue runs on milliseconds
	getQueueTime = time
WriteToLog "     ", "GetQueueTime ret:" & time
End Function


Sub QueuePop(queueIdx)
	if PupQueueEndPos(queueIdx) = -1 then exit sub 
	PupQueue(queueIdx, 0, 1 )=99
	SortPupQueue queueIdx
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),0 )=""
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),2 )=0
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),3 )=False
	PupQueueEndPos(queueIdx)=PupQueueEndPos(queueIdx)-1

'WriteToLog "     ", "--Q-Dump Pop---"
'	Dim xx
'	for xx = 0 to PupQueueEndPos(queueIdx)
'		'WriteToLog "     ", xx & " " & PupQueue(queueIdx, xx, 0) & " " & PupQueue(queueIdx, xx, 1) & " " & PupQueue(queueIdx, xx, 2) & " " & PupQueue(queueIdx, xx, 3) 
'	Next 
'WriteToLog "     ", "--Q-Dump Pop---"


End Sub

' NOTE - Dont Make msecLen=0 at the start because it immediatly triggers the clear/wait scene
Sub QueueScene(Command, msecLen, priority) 
	QueueScene2 0, Command, msecLen, priority, False 
End Sub

Sub QueueScene2(queueIdx, Command, msecLen, priority, bMustRun) 
WriteToLog "     ", "Queue Scene " & Command & " Len: " & msecLen
	if PupQueueEndPos(queueIdx) < UBound(PupQueue, 2) then 
		PupQueueEndPos(queueIdx)=PupQueueEndPos(queueIdx)+1
	End if 
	' NOTE: If it is full we overwrite the lowest priority (Optionally we could make the queue bigger)
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),0 )=Command & " '"
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),1 )=priority
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),2 )=msecLen
	PupQueue(queueIdx, PupQueueEndPos(queueIdx),3 )=bMustRun
	
	SortPupQueue queueIdx
	
'WriteToLog "     ", "--Q-Dump---"
'	Dim xx
'	for xx = 0 to PupQueueEndPos(queueIdx)
'		WriteToLog "     ", xx & " " & PupQueue(queueIdx, xx, 0) & " " & PupQueue(queueIdx, xx, 1) & " " & PupQueue(queueIdx, xx, 2) & " " & PupQueue(queueIdx, xx, 3)
'	Next 
'WriteToLog "     ", "--Q-Dump---"

	RunQueue queueIdx, True
End Sub

'*************************************


Sub tmrQueues_Timer(queueIdx)
'WriteToLog "     ", "TIMER!!!!!!!!!! " & queueIdx
	tmrQueues(queueIdx).Enabled = False 
	RunQueue queueIdx, False
End Sub 

Sub QueueSkip(queueIdx)						' Shortcycle the timer and move on 
'WriteToLog "     ", "--Q Skip--"
	if tmrQueues(queueIdx).Enabled Then
		tmrQueues(queueIdx).Enabled = False
		RunQueue queueIdx, False
	End if
End Sub

Sub QueueSkipNoDef(queueIdx)						' Shortcycle the timer and move on but dont start default
'WriteToLog "     ", "--Q Skip--"
	Dim SaveDef
	if tmrQueues(queueIdx).Enabled Then
		tmrQueues(queueIdx).Enabled = False
		SaveDef=PupQueueDefault(queueIdx)
		PupQueueDefault(queueIdx)=""
		RunQueue queueIdx, False
		PupQueueDefault(queueIdx)=SaveDef
	End if
End Sub

Sub RunQueue(queueIdx, bNewItem)
	dim qCmd, qTime
WriteToLog "     ", "Run Queue " &  queueIdx & " " & QueueActive(queueIdx) & " " & bNewItem & " " & Now
	if QueueActive(queueIdx) = False or bNewItem=False then 	' Nothing is running Or we just finished running something 
		if PupQueueEndPos(queueIdx) <> -1 then
			if PupQueueDefault(queueIdx)<>"" and QueueActive(queueIdx)=False and PupQueueDefaultClear(queueIdx)	<>"" then	' clear the default if there is one 
WriteToLog "     ", "Execute Clear:" & PupQueueDefaultClear(queueIdx)
				Execute PupQueueDefaultClear(queueIdx)
			End if
			QueueActive(queueIdx) = True
			qCmd=PupQueue(queueIdx, 0, 0)
			qTime=PupQueue(queueIdx, 0, 2)
WriteToLog "     ", "Exec " & qCmd
'			on error resume next 
			PupQueue(queueIdx, 0, 3)=True		' Set MustRun to True so it cant get deleted while running 
			Execute qCmd
'			if err.number <> 0 then MSGBox "EXEC Err: " & qCmd
'			On Error goto 0
'WriteToLog "     ", "Timer: " & qTime
			if qTime > 0 then 
				QueueCurrentTime(queueIdx) = DateAdd("s",qTime/1000, now)
'WriteToLog "     ", QueueCurrentTime(queueIdx)
				tmrQueues(queueIdx).Interval = qTime
				tmrQueues(queueIdx).Enabled = True
				'vpmtimer.addtimer cInt(qTime), "RunQueue False '"
				QueuePop(queueIdx)
			Else			' No timer just run the next item in the queue 
				QueueCurrentTime(queueIdx) = 0
				QueuePop(queueIdx)
				RunQueue queueIdx, False
			End If
		Else 
WriteToLog "     ", "Queue Empty Deactivated: " & PupQueueDefault(queueIdx)
			QueueActive(queueIdx) = False
			if PupQueueDefault(queueIdx)<>"" then 		' Run the default item
				Execute PupQueueDefault(queueIdx)
			End if 
		End If 
	End if
End Sub

Sub SortPupQueue(queueIdx)
	dim a, j, temp1, temp2, temp3, temp4
	for a = PupQueueEndPos(queueIdx) - 1 To 0 Step -1
		for j= 0 to a
			if PupQueue(queueIdx, j, 1)>PupQueue(queueIdx, j+1, 1) then		' Sort by priority 
				temp1=PupQueue(queueIdx, j+1,0 )
				temp2=PupQueue(queueIdx, j+1,1 )
				temp3=PupQueue(queueIdx, j+1,2 )
				temp4=PupQueue(queueIdx, j+1,3 )
				PupQueue(queueIdx, j+1,0 )=PupQueue(queueIdx, j,0 )
				PupQueue(queueIdx, j+1,1 )=PupQueue(queueIdx, j,1 )
				PupQueue(queueIdx, j+1,2 )=PupQueue(queueIdx, j,2 )
				PupQueue(queueIdx, j+1,3 )=PupQueue(queueIdx, j,3 )
				PupQueue(queueIdx, j, 0 )=temp1
				PupQueue(queueIdx, j, 1 )=temp2
				PupQueue(queueIdx, j, 2 )=temp3
				PupQueue(queueIdx, j, 3 )=temp4
			end if
		next
	next 

End Sub



'**************************************************
'******** Debug routines
Dim bDebugMode:bDebugMode = False		' Magna buttons perform debug functions
Dim debugEnableCount:debugEnableCount=0
Dim bDebugLeftMagnaDown:bDebugLeftMagnaDown=False
Dim bDebugRightMagnaDown:bDebugRightMagnaDown=False
dim DebugModeIndex:DebugModeIndex=-1
Sub HandleDebugDown(keycode)
	exit sub 

	Dim i
	Dim Ball
	Dim bFirst:bFirst=False
	dim bFound:bFound=False
	If KeyCode = LeftMagnaSave then bDebugLeftMagnaDown = True
	If KeyCode = RightMagnaSave then bDebugRightMagnaDown = True
WriteToLog "     ", "DEBUG L:" & bDebugLeftMagnaDown & " R:" & bDebugRightMagnaDown  & " cnt" & debugEnableCount
	if (bDebugMode = False) Then	' Turn on debug when you pres magnas together three times 
		if (bDebugLeftMagnaDown and bDebugRightMagnaDown) then
			debugEnableCount=debugEnableCount+1
			if debugEnableCount = 3 then 

				debugEnableCount=0
				if BallSearchCnt >0 then 	' TBD: If we have a ball stuck problem use this to move it to a good location
					For each Ball in GetBalls
'						if Ball.radius > 23 then 
'							Ball.x = 750
'							Ball.y = 1000
'							Ball.z = 25
'						End if 
					Next 
					PlaySoundVol "sfx-bat-phone1", VolDef
				End if

' Enable real debugging 
				PlaySoundVol "mk-GetOverHere1", 1
				bDebugMode = True
				DebugModeIndex=-1
			End If 
		End If 
		Exit Sub
	End if 

	if (bDebugLeftMagnaDown and bDebugRightMagnaDown = False) then 
		vpmtimer.addtimer 200, "HandleDebugLeft '"
	Elseif (bDebugLeftMagnaDown = False and bDebugRightMagnaDown) then 
		vpmtimer.addtimer 200, "HandleDebugRight '"
	elseif (bDebugLeftMagnaDown and bDebugRightMagnaDown) then 	' Setup for Wizard modes 
		' TBD Skip to higer modes here 
		if DebugModeIndex<4 then 
			DebugModeIndex=DebugModeIndex+1
			PlaySoundVol "mk-fatality1", VolDef

			Select case DebugModeIndex
				case 0:										' 2M2M
					Mode(CurrentPlayer, kModeIcarus)=1
					Mode(CurrentPlayer, kModeFear)=1
					Mode(CurrentPlayer, kModeAces)=1
					Mode(CurrentPlayer, kModeHallowed)=1
					Mode(CurrentPlayer, kModeRime)=1
					SetupEddieInserts
					RotateMode
				case 1:										' Madness

					TombTreasureCount(CurrentPlayer)=2
					StartTombTreasure(kTombShardRime)
					CheckTombTreasure

				case 2:  									' Number of the Beast 
					bTombTreasureReady(CurrentPlayer)=False		' Cancel Madness

					SetModeActive kModeEddie, False 
					EnableNOTB()

					ResetEDDIELetter
					SetModeActive kModeEddie, True
					SetupEddieInserts

				case 3:										' RUN TO THE HILLS 
					SetModeActive kModeNOTB, False		' Cancel NOTB
					ResetEDDIELetter
					SetModeActive kModeEddie, True
					SetupEddieInserts

					TombTreasureCount(CurrentPlayer)=9
					StartTombTreasure(kTombShardAces)
					CheckTombTreasure

				case 4:										' Cyborg Multiball
					EnableCyborg

			End Select 
		End if

	End If 
End Sub
Sub HandleDebugUp(keycode)
	exit sub 
	If KeyCode = LeftMagnaSave then bDebugLeftMagnaDown = False
	If KeyCode = RightMagnaSave then bDebugRightMagnaDown = False
End Sub

Sub HandleDebugLeft()
	Dim Ball
	if (bDebugLeftMagnaDown and bDebugRightMagnaDown = False) then 
		if BallsOnPlayfield = 1 then 
			For each Ball in GetBalls
				if INT(Ball.x)<>INT(CapKicker1.x) and INT(Ball.y)<>INT(CapKicker1.Y) and _
				   INT(Ball.x)<>INT(CapKicker2.x) and INT(Ball.y)<>INT(CapKicker2.Y) then 
'WriteToLog "     ", "Ball:" & INT(Ball.x) & " " & INT(Ball.y) & " " & INT(CapKicker1.x) & " " & INT(CapKicker1.Y)
					Ball.VelY = 0
					Ball.VelX = 0
					Ball.VelZ = 0
					Ball.x = 291
					Ball.y = 1855
					Ball.z = 25.00001
				End if 
			Next
		End If 
	End If 
End Sub
Sub HandleDebugRight()
	Dim Ball
	if (bDebugLeftMagnaDown = False and bDebugRightMagnaDown) then
		if BallsOnPlayfield = 1 then 
			For each Ball in GetBalls
				if INT(Ball.x)<>INT(CapKicker1.x) and INT(Ball.y)<>INT(CapKicker1.Y) and _
				   INT(Ball.x)<>INT(CapKicker2.x) and INT(Ball.y)<>INT(CapKicker2.Y) then 
					Ball.VelY = 0
					Ball.VelX = 0
					Ball.VelZ = 0
					Ball.x = 586.0515
					Ball.y = 1855
					Ball.z = 25.00009
				End if 
			Next
		End If 
	End If 
End Sub


'*****************************************************************************************************************************************
'  ERROR LOGS by baldgeek
'*****************************************************************************************************************************************

' Log File Usage:
'   WriteToLog "Label 1", "Message 1 "
'   WriteToLog "Label 2", "Message 2 "


Class DebugLogFile

    Private Filename
    Private TxtFileStream

	Public Sub LogInit()
		Filename = cGameName + "_debug_log.txt"      
        Set TxtFileStream = CreateObject("Scripting.FileSystemObject").OpenTextFile(Filename, 2, True)
	End Sub 

    Private Function LZ(ByVal Number, ByVal Places)
        Dim Zeros
        Zeros = String(CInt(Places), "0")
        LZ = Right(Zeros & CStr(Number), Places)
    End Function

    Private Function GetTimeStamp
        Dim CurrTime, Elapsed, MilliSecs
        CurrTime = Now()
        Elapsed = Timer()
        MilliSecs = Int((Elapsed - Int(Elapsed)) * 1000)
        GetTimeStamp = _
            LZ(Year(CurrTime),   4) & "-" _
            & LZ(Month(CurrTime),  2) & "-" _
            & LZ(Day(CurrTime),    2) & " " _
            & LZ(Hour(CurrTime),   2) & ":" _
            & LZ(Minute(CurrTime), 2) & ":" _
            & LZ(Second(CurrTime), 2) & ":" _
            & LZ(MilliSecs, 4)
    End Function

' *** WriteToLog "     ", the time with milliseconds, and a message of your choice
    Public Sub WriteToLog(label, message)
        TxtFileStream.WriteLine GetTimeStamp + " : " + label + " : " + message
		Debug.print label & " : " & message
	End Sub

End Class

Dim LogFileObj
Dim LogInit:LogInit=False
Sub WriteToLog(label, message)
	if KeepLogs Then
		if LogInit=False then NewLog()
		LogFileObj.WriteToLog label, message
	end if
End Sub

Sub NewLog()
	if KeepLogs Then
		LogInit=True
		Set LogFileObj = New DebugLogFile
		LogFileObj.LogInit
		LogFileObj.WriteToLog "NEW LOG", " "
	end if
End Sub


'*******************************************
'     FlippersPol  early 90's and after
'*******************************************


dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
InitPolarity
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'           FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, RF)
        dim x : for each x in a
                x.addpoint aStr, idx, aX, aY
        Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'WriteToLog "     ", Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'WriteToLog "     ", "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub


'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress1, RFPress1, LFCount1, RFCount1
dim LFState1, RFState1
dim LFEndAngle1, RFEndAngle1

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
LFEndAngle1 = Leftflipper1.endangle
RFEndAngle1 = RightFlipper1.endangle


Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then WriteToLog "     ", Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'WriteToLog "     ", "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'WriteToLog "     ", "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'WriteToLog "     ", "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

Sub RDampen_Timer()
	Cor.Update
End Sub








'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, cVolTable * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
    PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundVol(soundname, Volume)
  PlaySound soundname, 1, Volume
End Sub

Sub PlaySoundVolLoop(soundname, Volume)
  PlaySound soundname, -1, Volume
End Sub


Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************
Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 10
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
		PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
 	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
 	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
 		RandomSoundTargetHitWeak()
 	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
	TargetBouncer Activeball, 1
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub SoundOnOrbCollision()
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select
	On error Resume Next 
	PlaySoundAtLevelActiveBall snd, Vol(ActiveBall) * BallWithBallCollisionSoundFactor
	On Error goto 0
End Sub



'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Video Manager & skipper
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
'	Const pTopper=0			' show, 
'	Const pDMD=1			' ForceBack
'	Const pBackglass=2		' ForceBack
'	Const pPlayfield=3		' off
'	Const pMusic=4			' Music Only
'	Const pDMDFull=5		' 
'	Const pOverVid=11


	dim currentqueue
	dim cineon:cineon = 0
	dim skipped:skipped = 0
	dim bCancelNext:bCancelNext=False
	dim bMediaPaused(19)
	dim bMediaSet(19)
	bMediaPaused(0)=False:bMediaPaused(1)=False:bMediaPaused(2)=False:bMediaPaused(3)=False:bMediaPaused(4)=False:bMediaPaused(5)=False
	bMediaPaused(6)=False:bMediaPaused(7)=False:bMediaPaused(8)=False:bMediaPaused(9)=False:bMediaPaused(10)=False:bMediaPaused(11)=False
	bMediaPaused(12)=False:bMediaPaused(13)=False:bMediaPaused(14)=False:bMediaPaused(15)=False:bMediaPaused(16)=False:bMediaPaused(17)=False
	bMediaPaused(18)=False:bMediaPaused(19)=False
	bMediaSet(0)=False:bMediaSet(1)=False:bMediaSet(2)=False:bMediaSet(3)=False:bMediaSet(4)=False:bMediaSet(5)=False
	bMediaSet(6)=False:bMediaSet(7)=False:bMediaSet(8)=False:bMediaSet(9)=False:bMediaSet(10)=False:bMediaSet(11)=False
	bMediaSet(12)=False:bMediaSet(13)=False:bMediaSet(14)=False:bMediaSet(15)=False:bMediaSet(16)=False:bMediaSet(17)=False
	bMediaSet(18)=False:bMediaSet(19)=False
	sub pausemedia(channel) 

'WriteToLog "     ", "pause media ch:" & channel & " current:" & bMediaPaused(channel)
		if bMediaSet(channel) = False then Exit Sub 
		if bMediaPaused(channel) = False then
'WriteToLog "     ", "pause"
			PuPlayer.playpause channel
			bMediaPaused(channel)=True
		End If 
	End Sub

	sub resumemedia(channel) 
'WriteToLog "     ", "resume media ch:" & channel & " current:" & bMediaPaused(channel)
		if bMediaSet(channel) = False then Exit Sub 
		if bMediaPaused(channel) then
'WriteToLog "     ", "resume"
			PuPlayer.playresume channel
			bMediaPaused(channel)=False
		End If 
	End Sub
			
	sub playclear(chan)
		WriteToLog "     ", "play clear'd " & chan
		bMediaSet(chan) = False
		bMediaPaused(chan) = False

		if chan = pOverVid then 
			PuPlayer.PlayStop pOverVid
			'PuPlayer.SetLoop pOverVid, 0
		End If 

'		if chan = pAudio Then
'			PuPlayer.SetLoop pAudio, 0
'			PuPlayer.playstop pAudio
'		End If

		if chan = pDMDFull Then
			PuPlayer.SetLoop chan, 0
			PuPlayer.playstop chan
		End If

'		if chan = pBonusScreen then 
'			PuPlayer.SetBackGround chan, 0
'			PuPlayer.SetLoop chan, 0
'			PuPlayer.playstop chan
'		End If 

		if chan = pMusic Then
			PuPlayer.SetLoop pMusic, 0
			PuPlayer.playstop pMusic
			curSong=""
'			'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			if currentqueue <> "" then 
				bCancelNext = True
'WriteToLog "     ", "Clear is cancelling " & currentqueue
			End If 
			PuPlayer.SetBackGround pBackglass, 0
			PuPlayer.SetLoop pBackglass, 0
			PuPlayer.playstop pBackglass
		end if 
	end Sub

	Sub VolumeLower(channel, volume)
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&channel &", ""FN"":11, ""VL"":"&volume&" }"
	End Sub 
	
	Sub VolumeRestore(channel, volume)
		if volume = 1 Then
			if channel = pBackglass Then
				volume = VolBGMusic
			Elseif channel = pMusic Then
				volume = VolBGMusic
			Elseif channel = pDMDFull Then
				volume = VolBGVideo  
			end If
			volume = volume * 100
		end If

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&channel &", ""FN"":11, ""VL"":"&volume&" }"	
	End Sub 


	dim lastvocall:lastvocall=""
	dim noskipper:noskipper=0
	dim curSong
	sub playMusic(fileName)
'WriteToLog "     ", "playMusic: " & curSong & " " & fileName
		if curSong <> fileName then 
			playclear pMusic
			curSong=fileName
			playmedia fileName, MusicDir, pMusic, "", -1, "", 1, 1
		End if 
	end Sub 

	'example playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	sub playmedia(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Dim i
		bMediaSet(channel) = True
		if audiolevel = 1 Then
			if channel = pBackglass Then
				audiolevel = VolBGMusic
				currentqueue = ""
			Elseif channel = pMusic Then
				audiolevel = VolBGMusic
			Elseif channel = pDMDFull Then
				audiolevel = VolBGVideo  
			end If
			audiolevel = audiolevel * 100
		end If

'		if channel = pCallouts Then
'			if lastvocall = name then exit sub
'		end if
		

		if nextitem = "" Then
			If cinematic = "cineon" Then
				noskipper=1
				vpmtimer.addtimer length, "nextitems '"
			end If
		Else	
			currentqueue = "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			vpmtimer.addtimer length, "nextitems '"
			'vpmtimer.AddTimer length, "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			'vpmtimer.addtimer length, "nextitems '"
			'currentqueue = nextitem
		end If

		If cinematic = "cineon" and length <> -1 Then
'WriteToLog "     ", "ChangeVol pMusic for Cineon"

			skipped=0
			'PuPlayer.playpause 4 ' stop then resume the music
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":20 }"
			vpmtimer.addtimer length, "turnitbackupcine '"
			GiOff
			cineon = 1
		end If

WriteToLog "     ", "PlayMedia " & channel & " Dir:" & playlist & " File:" & name & " Vol:" & audiolevel & " Pri:" & priority & " Len:" & length
		PuPlayer.playlistplayex channel,playlist,name,audiolevel,priority
		if channel = pBackglass then PuPlayer.SetBackGround channel, 1
'		if channel = pAudio then PuPlayer.SetLoop channel, 1
		if channel = pMusic and length=-1 then PuPlayer.SetLoop channel, 1
'		if channel = pBonusScreen then PuPlayer.SetLoop channel, 1

'		If channel = pCallouts and length <> -1 Then
''WriteToLog "     ", "ChangeVol pBackglass Volume for Callouts"
'			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
'			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":60 }"
''			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":60 }"
'			vpmtimer.addtimer length, "turnitbackup'"
'		end If

		If channel = pOverVid and length <> -1 Then
'WriteToLog "     ", "ChangeVol pBackglass for OverVid" 
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":50 }"
			vpmtimer.addtimer length, "turnitbackupvid'"
		end If

'		if channel = pCallouts Then
'			lastvocall=name
'		end if


	end sub

	Sub turnitbackup
'WriteToLog "     ", "Change Volume5"

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &", ""FN"":11, ""VL"":"&VolMusic&" }"
'		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":"&VolSfx&" }"
		'WriteToLog "     ", "turnitbackup "
		'puplayer.setvolume pMusic sndtrkvol
	End Sub

	Sub turnitbackupvid
'WriteToLog "     ", "Change Volume4"

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic&" }"
		'WriteToLog "     ", "turnitbackupvid "
	End Sub

	Sub turnitbackupcine
'WriteToLog "     ", "Change Volume3"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":"&VolMusic&" }"
		'WriteToLog "     ", "turnitbackupcine "
	End Sub


	sub holder
	end sub

	sub nextitems
		if bCancelNext then 	' Cancel the last items that was sent to the queue
'WriteToLog "     ", "Cancel Queue"
			bCancelNext=False
			Exit Sub
		End If 
'WriteToLog "     ", "Executing " & currentqueue
		Execute currentqueue
		currentqueue = ""
		bCancelNext = False 	' Items in queue can trigger a cancel
'WriteToLog "     ", "Queue Empty"
'		if skipped = 0 Then
'			noskipper=0
'			'PuPlayer.playresume 4
''WriteToLog "     ", "Change Volume1"
'			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":"&VolMusic&" }"
'			gion
'			cineon = 0
'			'looktimer = 0
'			execute currentqueue
'		Elseif skipped = 1 Then
'			'PuPlayer.playresume 4
''WriteToLog "     ", "Change Volume2"
'			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":"&VolMusic&" }"
'			gion
'			cineon = 0
'			'looktimer = 0
'			skipped = 3
'			execute currentqueue
'			currentqueue = " "
'		end If
	end Sub

	sub vidskipper_timer		' TBD: Make this work (flippers skip cinematics)
		if cineon = 1  and noskipper = 0 Then
			if ldown = 1 and rdown = 1 Then
				nextitems
				skipped = 1
			end If
		end If
	end Sub

'*********************
'VR Mode
'*********************
DIM VRThings
If VRRoom > 0 Then
	cab_gion_Metals.visible = 0
	For each VRthings in VR_Pincab: VRthings.visible = 1 : Next
	If VRRoom = 1 Then
		for each VRThings in VR_BaSti:VRThings.visible = 1 : Next
		for each VRThings in VR_Min:VRThings.visible = 0 : Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Min:VRThings.visible = 1 : Next
		for each VRThings in VR_BaSti:VRThings.visible = 0 : Next
	End If
Else
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	For each VRthings in VR_Pincab: VRthings.visible = 0 : Next
	For each VRThings in VR_BaSti:VRThings.visible = 0 : Next
	if DesktopMode then 
		PinCab_Rails.visible = true 
	Else 
		PinCab_Rails.visible = false
	End If
End if

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < 100 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = -33 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub

' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

Randomize(21)
BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
if BeerBubble1.z > -771 then BeerBubble1.z = -955
BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
if BeerBubble2.z > -768 then BeerBubble2.z = -955
BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
if BeerBubble3.z > -768 then BeerBubble3.z = -955
BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
if BeerBubble4.z > -774 then BeerBubble4.z = -955
BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
if BeerBubble5.z > -771 then BeerBubble5.z = -955
BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
if BeerBubble6.z > -774 then BeerBubble6.z = -955
BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
if BeerBubble7.z > -768 then BeerBubble7.z = -955
BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************



'*****************************************************************	
'DOF Extra subs	
'*****************************************************************	
Sub TurnOffDofUndercab 'Simple sub to turn off any of the undercab effects when transitioning modes	
	DOF 200, DOFoff	
	DOF 201, DOFoff	
	DOF 202, DOFoff	
	DOF 203, DOFoff	
	DOF 204, DOFoff	
	DOF 205, DOFoff	
	DOF 206, DOFoff	
	DOF 207, DOFoff	
End Sub


'*****************************************************************	
'DOF Lookup Chart	
'	
'Special Thanks to RetroG33k, Shaggysrsg, Awalsh053, and LlamaStick for inital DOF testing	
'*****************************************************************	
'--------------------------------------	
'Flashers	
'--------------------------------------	
'Lower Left (white)- 150	
'Middle Right (blue)- 152	
'Top Right (wRGB), 153,154,155,156	
'--------------------------------------	
'Targets	
'--------------------------------------	
'Pharoah-1- 159	
'Pharoah-2- 160	
'Pharaoh-3- 161	
'Top Drop Target (lock)- 175	
'Middle Drop Target (orb)- 176	
'Bottom Drop Target (Bonus)- 177	
'--------------------------------------	
'Switches	
'--------------------------------------	
'L outlane-162	
'L inlane- 163	
'R outlane- 164	
'R inlane- 165	
'Left Ramp- 157	
'Right Ramp- 158	
'--------------------------------------	
'Switches	
'--------------------------------------	
'Left Outer Orbit-183	
'Left Inner Orbit-184	
'Right Inner Orbit-185	
'Right Outter Orbit-186	
'--------------------------------------	
'MUMMY Lights	
'--------------------------------------	
'M-210	
'U-211	
'M-212	
'M-213	
'Y-214	
'--------------------------------------	
'Other	
'--------------------------------------	
'DOF 126- award jackpot	
'DOF 127- Skillshot	
'DOF 121 -knocker effect	
'DOF 180- Extra Ball	
'Mystery Awarded- 151	
'Spinners (both)- 166	
'Ball indicator- 170	
'Ball shot- 171	
'Drained- 178	
'Tilt- 167	
'Underworld ramp up- 173	
'Revive Enabled- 172	
'Revive Used -174	
'Underworld ramp moving- 179	
'Coin Inserted- 220	
'--------------------------------------	
'Mechanical	
'--------------------------------------	
'Bumper 1- 107	
'Bumper 2- 111	
'Bumper 3- 109	
'101- Left Flipper	
'102- Right Flipper	
'103- Left Sling	
'105- Right Sling	
'--------------------------------------	
'Mode Undercab Scenarios	
'--------------------------------------	
'Attract- Multiple Flame Pattern- 200	
'Normal- Gold- 201	
'TwoMinutesToMidnight- Orange, 202	
'FlightOfIcarus- Red, 203	
'FearOfTheDark- Purple, 204	
'AcesHigh- Dodger Blue, 205	
'Hallowed- Orange, 206	
'Mariner- Cyan,207	
