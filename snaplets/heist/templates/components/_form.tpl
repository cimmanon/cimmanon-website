<dfLabel ref="type">Type</dfLabel>
<dfInputSelect ref="type" onchange="swapTags(this);" />

<dfLabel ref="date">Date</dfLabel>
<dfInput type="date" ref="date" required />

<dfLabel ref="date">Description</dfLabel>
<dfInputTextArea ref="description" required class="maximum" />

<dfSubView ref="tags"><fieldset>
	<legend>Tags</legend>

	<dfScriptValues ref="allTags" />

	<ul class="tags" id="tags">
		<dfCustomChoice ref="tags"><choice><li>
			<label><input type="checkbox" name="${path}" value="${value}" isChecked /> <name>blue</name></label>
		</li></choice></dfCustomChoice>
	</ul>
</fieldset></dfSubView>

<fieldset>
	<legend>Visibility</legend>

	<dfLabel ref="archived">Archived</dfLabel>
	<dfInputText ref="archived" /> <input type="button" value="Local" onclick="localPath('/${archivePath}${slug}/')" />

	<ul>
		<li><label><dfInputCheckbox ref="public" /> Public</label></li>
		<li><label><dfInputCheckbox ref="featured" /> Featured</label></li>
	</ul>
</fieldset>

<script type="text/javascript">
function localPath(prefix) {
	document.getElementById('form.archived').value = prefix + document.getElementById('form.date').value + '/';

}

var tags = document.getElementById('tags');
var currentTags = tags.getElementsByTagName('li');
var tagTemplate = currentTags[0];

function genTag(name) {
	var thisTag = tagTemplate.cloneNode(true);

	var thisLabel = thisTag.getElementsByTagName('label')[0];
	thisLabel.lastChild.nodeValue = " " + name;

	var thisInput = thisTag.getElementsByTagName('input')[0];
	var lastIndex = thisInput.value.lastIndexOf('.') + 1;
	var newName = thisInput.value.substr(0, lastIndex) + name;
	thisInput.value = name;
	thisInput.id = newName;
	thisInput.checked = false;

	return thisTag;
}

function swapTags(el) {
	var selectedType = el.options[el.selectedIndex].text;
	var newTags = allTags[selectedType];

	// convert existing tag nodes to use new values
	for (var i = 0; i < newTags.length; i++) {
		// if the new tag length is less than the old length, replace the contents
		if (i < currentTags.length) {
			var thisTag = currentTags[i];
			thisTag.parentNode.replaceChild(genTag(newTags[i]), thisTag);
		// otherwise, create new elements
		} else {
			tags.appendChild(genTag(newTags[i]));
		}
	}

	// remove extra tag nodes
	// have to remove from the end because currentTags is a live list,
	// otherwise we will end up deleting every other element
	while (i < currentTags.length) {
		tags.removeChild(currentTags[currentTags.length - 1]);
	}

	return true;
}
</script>
