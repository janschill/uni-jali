const svelte = require('svelte/compiler');
const svelteComponentFile = require('Button.svelte');
const svelteComponent = `
<script>
  let count = 0;

  function handleClick() {
    count += 1;
  }
</script>

<button on:click={handleClick}>
  Clicked {count} {count === 1 ? 'time' : 'times'}
</button>
`;

const compiledSvelte = svelte.compile(svelteComponentFile);
const parsedSvelte = svelte.parse(svelteComponent);
console.log(compiledSvelte.js.code);
