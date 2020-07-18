$version = "emacs-26.3-x86_64"

Invoke-WebRequest -Uri "https://ftp.gnu.org/gnu/emacs/windows/emacs-26/$version.zip" -OutFile ".\$version.zip" 

Expand-Archive ".\$version.zip" -DestinationPath ".\$version\"

$dotEmacs = Resolve-Path -Path ".\..\..\init.el"
$dotEmacs = "$dotEmacs".Replace("\", "/")

$loadFile = "(load-file `"$dotEmacs`")"
$loadFile | Set-Content "C:/Users/$env:UserName/AppData/Roaming/.emacs"

$bin = Resolve-Path -Path ".\$version\bin"
$linkPath = "C:\ProgramData\Microsoft\Windows\Start Menu\Programs\"

$objShell = New-Object -ComObject ("WScript.Shell")
$objShortCut = $objShell.CreateShortcut($linkPath + "\Emacs.lnk")
$objShortCut.TargetPath="$bin\runemacs.exe"
$objShortCut.Save()

<# $emacsClient = "$bin\emacsclientw.exe"

cmd /c assoc .ps1=powershellfile
cmd /c ftype powershellfile=$emacsClient "%1"
cmd /c assoc .org=orgfile
cmd /c ftype orgfile=$emacsClient "%1"
cmd /c assoc .org=orgfile
cmd /c ftype orgfile=$emacsClient "%1"
cmd /c assoc .py=pyfile
cmd /c ftype pyfile=$emacsClient "%1"
cmd /c assoc .clj=cljfile
cmd /c ftype cljfile=$emacsClient "%1"
cmd /c assoc .el=elfile
cmd /c ftype elfile=$emacsClient "%1" #>
