<img src="https://img.shields.io/badge/status-startup-75441a" align="right"/>

Ներկայացումներ հավանական երևույթների շուրջ, կիրառական ապարատների մասնակցությամբ։

### Local Development

#### Setup

```sh
npx elm-tooling install
```

#### Testing

Փորձերի ու չափումների համար աշխատացրեք տեստերը

```sh
npx elm-test
```

Ընթացքում տեստերի արդյունքները շարունակաբար տեսնելու համար սեղմեք

```sh
npx elm-test --watch
```

#### Running in browser

Ընթացքում ծրագրի աշխատանքին հետևելու համար սեղմեք

```sh
npx elm-watch@beta hot
```

ապա բացեք տրամադրված լինկը բրաուզերով

### Contributing

#### Introducing changes

Կոդը կարելի ա փոխել միանգամից հիմնական բռենչի վրա, բայց ընկերներով աշխատելու դեպքում կարա ցանկալի լինի Pull Request-ներով, որպես նայել քննարկելու հարմար[^1] վարիանտ։

Ամեն դեպքում փոփոխությունները պետք ա լինեն կոնկրետ, ու հնարավորինս փոքր։ Ավելի մանրամասն commit-ների մասին՝ [տես հավելվածը](https://github.com/amotali/konvektion/blob/main/Commit%20Messages.md)։

[^1]: Որպես հարմարություն, Pull Request-ների վրա միացրած ա [որակավորման ավտոմատիկա](/actions/workflows/audit.yml)՝ տեստերը, կոդի ձևաչափը, և նման բաներ ստուգելու համար։

#### Publishing online

[Պուբլիկացիայի ավտոմատիկան](/actions/workflows/publishing.yml) թարմացնում ա [սայտը](https://antaranyan.github.io/cycle-2-spatial-distribution/) հիմնական `main` բռենչի վրա փոփոխությունների ժամանակ։

<details>
<summary>Եթե այլ բռենչերից ա պետք, էլի հնարավոր ա գնալ ձեռով գործարկել</summary>

1. Navigate to the [Publishing action tab](/actions/workflows/publishing.yml) in GitHub Actions
2. Select the appropriate workflow and branch
3. Click the "Run workflow" button (կոճակը)

![image](https://user-images.githubusercontent.com/25614707/222993363-38a820ed-2e94-4bd1-b566-9ff312b3d270.png)

</details>

