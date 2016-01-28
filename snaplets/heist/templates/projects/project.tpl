<bind tag="pageTitle">Project <name>Project Name</name></bind>
<apply template="/_layout">
<h1><name>Project Name</name></h1>

<apply template="_project_details" />

<section class="major project">
	<h1>History</h1>

	<div class="listing">
		<component><article>
			<apply template="/components/_component" />

			<image><img src="/screenshots/${slug}/${filename}" width="${width}" height="${height}" alt="" /></image>
		</article></component>
	</div>

	<image><aside>
		<img src="/screenshots/${slug}/${filename}" width="${width}" height="${height}" alt="" />
	</aside></image>
</section>
</apply>
