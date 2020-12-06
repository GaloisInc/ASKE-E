import React from 'react';

import { Container } from 'react-bootstrap';

import { HelloD3 } from '../components/HelloD3';

import '../styles/App.scss';

const App: React.FC = () => {
    return (
        <Container className='App' fluid data-testid='app-test'>
            <HelloD3 id='hello-d3' />
        </Container>
    );
};

export default App;
