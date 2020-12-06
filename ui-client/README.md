# ASKE-E Demo UI Client

This is just a simple React/Bootstrap/D3 UI for demoing ASKE-E functionality.

## Getting Started

### Node/nvm

You will need to have [Node](https://nodejs.org/en/) installed. The recommended way to do this is via [nvm](https://github.com/nvm-sh/nvm). It is recommended that you set it up to use the latest long-term-stable release. This can be done via the command:

```sh
$ nvm install 'lts/*' --reinstall-packages-from=current
```

### Installation

Once you have node installed, you should be able to install all the necessary dependencies via the following:

```sh
$ npm install
```

After that, you should be able to run;

```sh
$ npm start
```

Once the app starts up, it should automatically open a browser window with the app and the browser "developer tool" panel.

## Development Environment

If you are using VSCode, you may see errors like `Cannot use JSX unless the '--jsx' flag is provided`. If you see this, you can follow these [SO steps](https://stackoverflow.com/questions/50432556/cannot-use-jsx-unless-the-jsx-flag-is-provided) to resolve that issue.

## Basic Application Structure

The application structure is:

```
/public - browser assets for basic HTML page housing the app
/src
  |- /api (client api layer)
  |- /assets (any SVGs or other static assets)
  |- /components (single-purpose, simple React components)
  |- /containers ("page" level React components)
  |- /styles (SASS stylesheets for customizing look/feel)
  |- index.tsx (root React component for the app)
  |- react-app-env.d.ts (TypeScript declaration created by create-react-app)
  |- reportWebVitals.ts (boilerplate page statistics tooling created by create-react-app)
  |- setupTests.ts (boilerplate to set up Jest)
package.json (project configuration)
tsconfig.json (TypeScript configuration)
README.md (this file)
```

## TypeScript

This application uses (TypeScript)[https://www.typescriptlang.org/]. However, you can write vanilla JavaScript/React (using `.js` and `.jsx` files respectively)

## Jest

Create-react-app installs [Jest](https://jestjs.io/) for testing.

## Prettier

[Prettier](https://prettier.io/docs/en/index.html) has been set up for basic formatting. If you are using VSCode, you can configure it via tips [here](https://github.com/prettier/prettier-vscode).

## Bootstrap

[React Bootstrap](https://react-bootstrap.github.io/) has been installed for a base set of components for crafing the UI. It is built on top of [bootstrap 4](https://getbootstrap.com/docs/4.0/getting-started/introduction/) which uses [SASS](https://sass-lang.com/)

## Create React App

This project was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).

## Available Scripts

In the project directory, you can run:

#### `npm start`

Runs the app in the development mode.\
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.\
You will also see any lint errors in the console.

#### `npm test`

Launches the test runner in the interactive watch mode.\
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

#### `npm run build`

Builds the app for production to the `build` folder.\
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.\
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

#### `npm run eject`

**Note: this is a one-way operation. Once you `eject`, you can’t go back!**

If you aren’t satisfied with the build tool and configuration choices, you can `eject` at any time. This command will remove the single build dependency from your project.

Instead, it will copy all the configuration files and the transitive dependencies (webpack, Babel, ESLint, etc) right into your project so you have full control over them. All of the commands except `eject` will still work, but they will point to the copied scripts so you can tweak them. At this point you’re on your own.

You don’t have to ever use `eject`. The curated feature set is suitable for small and middle deployments, and you shouldn’t feel obligated to use this feature. However we understand that this tool wouldn’t be useful if you couldn’t customize it when you are ready for it.

### Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

To learn React, check out the [React documentation](https://reactjs.org/).
