export { Console as ConsoleImports } from './imports/console';
export function render(lang: Lang, wit: string, options: Options): Files;
export type Files = [string, string][];
/**
* # Variants
* 
* ## `"js"`
* 
* ## `"rust"`
* 
* ## `"java"`
* 
* ## `"wasmtime"`
* 
* ## `"c"`
* 
* ## `"markdown"`
*/
export type Lang = 'js' | 'rust' | 'java' | 'wasmtime' | 'c' | 'markdown';
export interface Options {
  rustUnchecked: boolean,
  wasmtimeTracing: boolean,
  jsCompat: boolean,
  jsInstantiation: boolean,
}
