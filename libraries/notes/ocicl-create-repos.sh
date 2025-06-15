#!/bin/sh

set -x

cd ~/Work/ocicl-admin

gh repo create ocicl/$1 --public --clone

cd $1

echo *~ > .gitignore
mkdir -p .github/workflows
cat > .github/workflows/main.yml <<END
on: [push, workflow_dispatch]

jobs:
  ocicl_job:
    permissions:
      packages: write
    runs-on: ubuntu-latest
    name: Test and publish package
    steps:
      - id: build-and-publish
        uses: ocicl/ocicl-action@main
        with:
          gpg_signing_key: \${{ secrets.GPG_SIGNING_KEY }}
          gpg_public_key: \${{ secrets.GPG_PUBLIC_KEY }}
          dockerhub_password: \${{ secrets.DOCKERHUB_PASSWORD }}
          llm_api_key: \${{ secrets.LLM_API_KEY }}
END

cat > LICENSE <<END
--------------------------------------------------------------------------------
This license pertains to the files within this repository, and does
not apply to software that is merely referenced by, but not
incorporated into, this repository.
--------------------------------------------------------------------------------

MIT License

Copyright (c) 2024 ocicl hackers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
END

cat > README.org <<END
* $1



|---------+-------------------------------------------|
| source  | git: |
| commit  | |
| systems | $1 |
|---------+-------------------------------------------|

END
