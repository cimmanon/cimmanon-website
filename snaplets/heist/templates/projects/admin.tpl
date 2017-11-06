<bind tag="pageTitle">Administrate Projects</bind>
<apply template="/_admin">
<h1>Projects</h1>

<table class="spreadsheet">
	<thead>
		<tr>
			<th>Name</th>
			<th>URL</th>
			<th>Featured</th>
			<th>Edit</th>
			<th>Components</th>
		</tr>
	</thead>

	<tbody>
		<project><tr>
			<td><a href="/projects/${slug}/"><name>Project</name></a></td>
			<td><url><a href="${href}">URL</a></url></td>
			<td class="${featured} bool"><featured>Featured</featured></td>
			<td><a href="./${slug}/">Edit</a></td>
			<td><a href="./${slug}/components/">Components</a></td>
		</tr></project>
	</tbody>
</table>
</apply>
