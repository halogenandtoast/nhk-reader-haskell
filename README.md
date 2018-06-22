# NhkReader

## About

This application is designed to be a news aggregator for NHK Easy News. It's goal is to provide an easy to read format for mobile devices and potentially add additional features for non-native speakers to better learn the Japanese language.

## Setting up development environment

### Dependencies

* Postgresql
* Redis

### Database Setup

After installing Postgres, run:

```
createuser yesod --password yesod --superuser
createdb nhk_reader_development
```

### Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Run Development Server

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Deployment

```
docker build . -t nhk
heroku container:push web
heroku container:release web
```
