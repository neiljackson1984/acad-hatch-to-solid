default: data.lsp
	autohotkey reloadCurrentAutocadFile.ahk
	autohotkey executeInAutocad.ahk generateDeliverableDrawing.lsp
	
data.lsp: doorData.json windowData.json roomData.json convertJsonToLisp.php
	php convertJsonToLisp.php

