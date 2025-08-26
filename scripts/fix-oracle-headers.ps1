param(
  [string]$Root = "."
)

$okDir  = Join-Path $Root "tests/lex/ok"
$errDir = Join-Path $Root "tests/lex/err"

function Fix-Expect {
  param(
    [Parameter(Mandatory)] [string]$Path,
    [Parameter(Mandatory)] [ValidateSet("OK","ERR")] [string]$Mode
  )

  $text  = if (Test-Path $Path) { Get-Content -Raw -Encoding utf8 -Path $Path } else { "" }
  $lines = $text -split "`r?`n", 0

  # Find first non-blank, non-comment line
  $firstIdx = -1
  for ($i = 0; $i -lt $lines.Length; $i++) {
    $t = $lines[$i].Trim()
    if ($t -ne '' -and -not $t.StartsWith('#')) { $firstIdx = $i; break }
  }

  $needHeader = $true
  if ($firstIdx -ge 0) {
    $first = $lines[$firstIdx].Trim()
    if ($first -eq 'OK' -or $first -eq 'ERR') { $needHeader = $false }
  }

  $out = New-Object System.Collections.Generic.List[string]
  if ($needHeader) { $out.Add($Mode) }

  foreach ($line in $lines) {
    if ($line -match '^\s*-\s*Intent:') {
      $trimmed = $line.TrimStart()
      $out.Add('# ' + $trimmed)
    } else {
      $out.Add($line)
    }
  }

  $final = [string]::Join("`r`n", $out)
  Set-Content -Path $Path -Value $final -Encoding utf8
}

if (Test-Path $okDir)  { Get-ChildItem $okDir  -Recurse -Filter *.expect.txt -File | ForEach-Object { Fix-Expect -Path $_.FullName -Mode 'OK'  } }
if (Test-Path $errDir) { Get-ChildItem $errDir -Recurse -Filter *.expect.txt -File | ForEach-Object { Fix-Expect -Path $_.FullName -Mode 'ERR' } }

Write-Host "Fixed oracle headers."
