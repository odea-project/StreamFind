import ReactDOM from "react-dom";
import Demo from './components/Demo'
import { DndProvider } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'
import '@fontsource/roboto/300.css';
import '@fontsource/roboto/400.css';
import '@fontsource/roboto/500.css';
import '@fontsource/roboto/700.css';
import {createRoot} from "react-dom/client";

function App() {
	return (
		<div className="App">
			<DndProvider backend={HTML5Backend}>
				<Demo />
			</DndProvider>
		</div>
	)
}

const root = createRoot(document.getElementById('root'));
root.render(<App />);