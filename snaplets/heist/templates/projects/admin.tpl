<apply template="/_layout">
<h1>Project Administration</h1>

<div class="listing">
	<section>
		<h1>Current Projects</h1>

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
					<td><a href="./project/${slug}/">Edit</a></td>
					<td><a href="./project/${slug}/components/">Components</a></td>
				</tr></project>
			</tbody>
		</table>
	</section>

	<section>
		<h1>New Project</h1>

		<dfForm class="simplified">
			<apply template="_form" />
			<input type="submit" value="Add" />
		</dfForm>
	</section>
</div>
</apply>