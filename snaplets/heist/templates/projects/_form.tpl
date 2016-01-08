<dfLabel ref="name">Name</dfLabel>
<dfInputText ref="name" required />

<dfLabel ref="description">Description</dfLabel>
<dfInputTextArea ref="description" required class="maximum" />

<dfLabel ref="slug">Slug</dfLabel>
<dfInputText ref="slug" required pattern="^[\w\-\_\.]*$" />

<dfLabel ref="url">URL</dfLabel>
<dfInput type="url" ref="url" />

<ul>
	<li><label><dfInputCheckbox ref="featured" /> Featured?</label></li>
</ul>
