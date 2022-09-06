Invoke-WebRequest -Uri "https://corwin.bru.st/emacs-28/emacs-28.1-NATIVE_FULL_AOT.zip" -OutFile "$env:temp\emacs.zip"

if (Test-Path -Path "C:\Emacs") {
    Get-ChildItem "C:\Emacs" -Recurse | Remove-Item -Force -Recurse -Confirm:$false -ErrorAction SilentlyContinue
}

#Remove-Item "C:\Emacs" -Recurse -Force -Confirm:$false -ErrorAction SilentlyContinue
Expand-Archive "$env:temp\emacs.zip" -DestinationPath "C:\Emacs"
Remove-Item "$env:temp\emacs.zip"

$dotEmacs = Resolve-Path -Path ".\..\..\init.el"
$dotEmacs = "$dotEmacs".Replace("\", "/")

$loadFile = "(load-file `"$dotEmacs`")"
$loadFile | Set-Content "C:/Users/$env:UserName/AppData/Roaming/.emacs"

$bin = Resolve-Path -Path "C:\Emacs\bin"
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
